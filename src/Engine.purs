module Engine where

import Prelude
import Data.TypedArray as T
import Graphics.WebGL.Raw as GL
import Graphics.WebGL.Raw.Enums as GLE
import Graphics.WebGL.Raw.Types as GLT
import Compiler (flattenParZn, compileShaders)
import Config (AudioAnalyser, Epi, EpiS, Module, Pattern, EngineST, EngineConf, SystemST, SystemConf)
import Control.Monad (when)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (lift)
import Control.Monad.ST (writeSTRef, STRef, newSTRef, modifySTRef, readSTRef)
import Data.Array (length, concatMap, (!!), (..), zip, foldM)
import Data.Complex (Cartesian(..), inCartesian)
import Data.Either (either)
import Data.Int (toNumber, fromNumber)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (StrMap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), snd, fst)
import Graphics.Canvas (setCanvasHeight, setCanvasWidth, getCanvasElementById)
import Graphics.WebGL (runWebgl, debug)
import Graphics.WebGL.Context (getWebglContextWithAttrs, defaultWebglContextAttrs)
import Graphics.WebGL.Methods (uniform2fv, uniform1fv, drawArrays, uniform1f, clearColor, vertexAttribPointer, enableVertexAttribArray, bufferData, bindBuffer, createBuffer, createFramebuffer, createTexture)
import Graphics.WebGL.Raw.Types (ArrayBufferView)
import Graphics.WebGL.Shader (getUniformBindings, getAttrBindings, compileShadersIntoProgram)
import Graphics.WebGL.Types (WebGL, WebGLContext, WebGLProgram, WebGLTexture, WebGLFramebuffer, ArrayBufferType(ArrayBuffer), BufferData(DataSource), BufferUsage(StaticDraw), DataType(Float), DrawMode(Triangles), Uniform(Uniform), WebGLError(ShaderError))
import Util (lg, replaceAll, unsafeNull)

foreign import audioData :: forall eff. AudioAnalyser -> Eff eff (ArrayBufferView)
foreign import initAudioAnalyzer :: forall eff. Int -> Eff eff AudioAnalyser

-- PUBLIC

-- get a webgl texture, set default properties
getTex :: WebGL WebGLTexture
getTex = do
  ctx <- ask
  tex <- createTexture
  liftEff $ GL.bindTexture ctx GLE.texture2d tex
  liftEff $ GL.texParameteri ctx GLE.texture2d GLE.textureWrapS GLE.clampToEdge
  liftEff $ GL.texParameteri ctx GLE.texture2d GLE.textureWrapT GLE.clampToEdge
  liftEff $ GL.texParameteri ctx GLE.texture2d GLE.textureMinFilter GLE.linear
  liftEff $ GL.texParameteri ctx GLE.texture2d GLE.textureMagFilter GLE.linear
  -- use mipmaps?
  debug

  return tex


-- initialize framebuffer/texture pair
initTex :: Int -> WebGL (Tuple WebGLTexture WebGLFramebuffer)
initTex dim = do
  ctx <- ask
  tex <- getTex
  fb <- createFramebuffer
  liftEff $ GL.texImage2D_ ctx GLE.texture2d 0 GLE.rgba dim dim 0 GLE.rgba GLE.unsignedByte (unsafeNull :: GLT.ArrayBufferView)
  liftEff $ GL.bindFramebuffer ctx GLE.framebuffer fb
  liftEff $ GL.framebufferTexture2D ctx GLE.framebuffer GLE.colorAttachment0 GLE.texture2d tex 0
  debug

  return $ Tuple tex fb


-- initialize auxiliary textures
initAux :: EngineConf -> WebGLContext -> GLT.TexImageSource -> WebGL (Array WebGLTexture)
initAux engineConf ctx empty = do
  --traverse (\_ -> getTex) (0..(engineConf.numAux - 1))
  traverse handle (0..(engineConf.numAux - 1))
  where
    handle i = do
      aux <- getTex
      liftEff $ GL.bindTexture ctx GLE.texture2d aux
      liftEff $ GL.texImage2D ctx GLE.texture2d 0 GLE.rgba GLE.rgba GLE.unsignedByte empty
      return aux


-- initialize audio texture
initAudio :: EngineConf -> WebGLContext -> GLT.TexImageSource -> WebGL (Maybe (Tuple WebGLTexture AudioAnalyser))
initAudio engineConf ctx empty = do
  case engineConf.audioAnalysisEnabled of
    false -> return Nothing
    true -> do
      audioTex <- getTex
      analyser <- lift $ lift $ initAudioAnalyzer engineConf.audioBufferSize
      return $ Just (Tuple audioTex analyser)



-- upload aux textures
uploadAux :: forall eff. EngineST -> String -> Array String -> Epi eff (Array String)
uploadAux es host names = do
  case es.aux of
    Nothing -> throwError "aux textures not initialized"
    (Just aux) -> do
      when (length aux < length names) do
        throwError "not enough aux textures"

      foldM (createImage es.ctx host) 0 (zip aux names)
      return names


-- create an image object. can throw error if images missing!  also some synchronization issues
createImage :: forall eff. WebGLContext -> String -> Int -> (Tuple WebGLTexture String) -> Epi eff Int
createImage ctx host c (Tuple aux name) = do
  lift $ createImageImpl (host ++ name) \img -> do
    runWebgl (do
      liftEff $ GL.bindTexture ctx GLE.texture2d aux
      liftEff $ GL.texImage2D ctx GLE.texture2d 0 GLE.rgba GLE.rgba GLE.unsignedByte img
    ) ctx
    return unit
  return $ c + 1


foreign import createImageImpl :: forall eff. String ->
                                  (GLT.TexImageSource -> Eff eff Unit) ->
                                  Eff eff Unit


clearFB :: forall eff h. EngineConf -> EngineST -> EpiS eff h Unit
clearFB engineConf engineST = do
  let ctx = engineST.ctx
  execGL ctx do
    liftEff $ GL.bindTexture ctx GLE.texture2d $ fst $ fromJust engineST.tex
    liftEff $ GL.texImage2D ctx GLE.texture2d 0 GLE.rgba GLE.rgba GLE.unsignedByte engineST.empty
    liftEff $ GL.bindTexture ctx GLE.texture2d $ snd $ fromJust engineST.tex
    liftEff $ GL.texImage2D ctx GLE.texture2d 0 GLE.rgba GLE.rgba GLE.unsignedByte engineST.empty
  return unit

foreign import emptyImage :: forall eff. Int -> Eff eff GLT.TexImageSource

-- compile shaders and load into systemST
setShaders :: forall eff h. SystemConf -> EngineConf -> STRef h EngineST -> SystemST h -> Pattern -> EpiS eff h Unit
setShaders sysConf engineConf esRef sys pattern = do
  es <- lift $ readSTRef esRef

  -- load & compile shaders
  {main, disp, vert, aux} <- compileShaders pattern sys
  let mainF = replaceAll "\\$fract\\$" (show engineConf.fract) main -- a little ghetto, we need this in a for loop

  auxImg <- uploadAux es sysConf.host aux

  Tuple main' disp' <- execGL es.ctx ( do
    -- create programs
    mainProg <- compileShadersIntoProgram vert mainF
    dispProg <- compileShadersIntoProgram vert disp

    -- vertex coords
    pos <- createBuffer
    bindBuffer ArrayBuffer pos
    bufferData ArrayBuffer (DataSource (T.asFloat32Array [-1.0,-1.0,1.0,-1.0,-1.0,1.0,
                                                          -1.0,1.0,1.0,-1.0,1.0,1.0])) StaticDraw

    dispAttr <- getAttrBindings dispProg
    mainAttr <- getAttrBindings mainProg

    enableVertexAttribArray mainAttr.a_position
    vertexAttribPointer mainAttr.a_position 2 Float false 0 0
    enableVertexAttribArray dispAttr.a_position
    vertexAttribPointer dispAttr.a_position 2 Float false 0 0

    debug

    return $ Tuple mainProg dispProg
  )

  lift $ modifySTRef esRef (\s -> s {dispProg = Just disp', mainProg = Just main', auxImg = auxImg})
  return unit


-- initialize the rendering engine & create state.  updates an existing state if passed
-- maybe validate that kernelDim > 0?
initEngineST :: forall eff h. SystemConf -> EngineConf -> SystemST h -> Pattern -> String -> Maybe (STRef h EngineST) -> EpiS eff h (STRef h EngineST)
initEngineST sysConf engineConf sys pattern canvasId esRef' = do
  -- find canvas & create context
  canvasM <- liftEff $ getCanvasElementById canvasId
  canvas <- case canvasM of
    Just c -> return c
    Nothing -> throwError $ "init engine - canvas not found: " ++ canvasId

  let attrs = defaultWebglContextAttrs {
        alpha =                 false
      , depth =                 false
      , antialias =             false
      , preserveDrawingBuffer = true}

  ctxM <- liftEff $ getWebglContextWithAttrs canvas attrs
  ctx <- case ctxM of
    Just c -> return c
    Nothing -> throwError "Unable to get a webgl context!!!"

  empty <- lift $ emptyImage engineConf.kernelDim

  -- get reference
  esRef <- case esRef' of
    Just ref -> do
      lift $ modifySTRef ref (\r -> r {empty = empty})
      return ref
    Nothing -> do
      let tmp = {dispProg: Nothing, mainProg: Nothing, tex: Nothing, fb: Nothing, aux: Nothing, audio: Nothing, auxImg: [], ctx: ctx, empty}
      lift $ newSTRef tmp

  es <- lift $ readSTRef esRef

  -- if we change kernel_dim we need to redo this
  let dim = engineConf.kernelDim
  lift $ setCanvasWidth (toNumber dim) canvas
  lift $ setCanvasHeight (toNumber dim) canvas

  -- webgl initialization
  res <- execGL ctx do
    Tuple tex0 fb0 <- initTex dim
    Tuple tex1 fb1 <- initTex dim
    aux <- initAux engineConf es.ctx empty
    audio <- initAudio engineConf es.ctx empty

    clearColor 0.0 0.0 0.0 1.0
    liftEff $ GL.clear ctx GLE.colorBufferBit
    liftEff $ GL.viewport ctx 0 0 dim dim

    return $ es {
        fb  = Just (Tuple fb0 fb1)
      , tex = Just (Tuple tex0 tex1)
      , aux = Just aux
      , audio = audio
    }

  -- set shaders
  lift $ writeSTRef esRef res
  setShaders sysConf engineConf esRef sys pattern

  return esRef


-- do the thing!
renderFrame :: forall eff h. SystemST h -> EngineConf -> EngineST -> Pattern -> Int -> EpiS eff h Unit
renderFrame systemST engineConf engineST pattern frameNum = do
  let ctx = engineST.ctx

  -- unpack
  tex <- case engineST.tex of
    Just x -> return x
    Nothing -> throwError "RenderFrame: missing textures"
  fbs <- case engineST.fb of
    Just x -> return x
    Nothing -> throwError "RenderFrame: missing framebuffers"
  aux <- case engineST.aux of
    Just x -> return x
    Nothing -> throwError "RenderFrame: missing aux"
  main <- case engineST.mainProg of
    Just x -> return x
    Nothing -> throwError "RenderFrame: missing main program"
  disp <- case engineST.dispProg of
    Just x -> return x
    Nothing -> throwError "RenderFrame: missing disp program"

  -- bind par & zn
  bindParZn systemST.moduleRefPool ctx main pattern.main
  bindParZn systemST.moduleRefPool ctx disp pattern.disp

  execGL ctx do
    liftEff $ GL.useProgram ctx main

    -- bind main uniforms
    mainUnif <- getUniformBindings main
    uniform1f mainUnif.time (systemST.t - pattern.tPhase)
    uniform1f mainUnif.kernel_dim (toNumber engineConf.kernelDim)

    -- aux
    when (length engineST.auxImg > 0) do
      auxU <- liftEff $ GL.getUniformLocation ctx main "aux"
      case auxU of
        Just auxU' -> liftEff $ GL.uniform1iv ctx auxU' (1..(length engineST.auxImg))
        Nothing   -> throwError $ ShaderError "missing aux uniform!"
      liftEff $ forE 0.0 (toNumber $ length engineST.auxImg) \i -> do
        let i' = fromJust $ fromNumber i
        GL.activeTexture ctx (GLE.texture1 + i')
        GL.bindTexture ctx GLE.texture2d $ fromJust (aux !! i')

    --audio info
    case engineST.audio of
      Just (Tuple audioTex analyser) -> do
        liftEff $ GL.bindTexture ctx GLE.texture2d audioTex
        dta <- lift $ lift $ audioData analyser
        liftEff $ GL.texImage2D_ ctx GLE.texture2d 0 GLE.alpha engineConf.audioBufferSize 2 0 GLE.alpha GLE.unsignedByte dta

        audioU <- liftEff $ GL.getUniformLocation ctx main "audio"
        case audioU of
          Just audioU' -> do
            let ofs = length engineST.auxImg + 1
            liftEff $ GL.uniform1i ctx audioU' ofs
            liftEff $ GL.activeTexture ctx (GLE.texture0 + ofs)
            liftEff $ GL.bindTexture ctx GLE.texture2d audioTex
          Nothing   -> throwError $ ShaderError "missing audio uniform!"
      Nothing -> return unit


    -- ping pong buffers
    let tm = if frameNum `mod` 2 == 0 then fst tex else snd tex
    let td = if frameNum `mod` 2 == 1 then fst tex else snd tex
    let fb = if frameNum `mod` 2 == 1 then fst fbs else snd fbs

    -- draw
    liftEff $ GL.activeTexture ctx GLE.texture0
    liftEff $ GL.bindTexture ctx GLE.texture2d tm
    liftEff $ GL.bindFramebuffer ctx GLE.framebuffer fb
    drawArrays Triangles 0 6

    debug

    -- disp/post program
    liftEff $ GL.useProgram ctx disp

    -- bind disp uniforms
    dispUnif <- getUniformBindings disp
    uniform1f dispUnif.kernel_dim (toNumber engineConf.kernelDim)

    -- draw
    liftEff $ GL.bindTexture ctx GLE.texture2d td
    liftEff $ GL.bindFramebuffer ctx GLE.framebuffer unsafeNull
    drawArrays Triangles 0 6

    debug


-- bind parameters & zn values from pattern into program
bindParZn :: forall h eff. StrMap (STRef h Module) -> WebGLContext -> WebGLProgram -> String -> EpiS eff h Unit
bindParZn lib ctx prog n = do
  {lib: _, par, zn} <- flattenParZn {lib, par: [], zn: []} n
  let znC = map inCartesian zn
  let znA = concatMap fn znC

  execGL ctx do
    liftEff $ GL.useProgram ctx prog
    unif <- getUniformBindings prog

    when (length par > 0) do
      mParU <- liftEff $ GL.getUniformLocation ctx prog "par"
      case mParU of
        Just parU -> uniform1fv (Uniform parU) (T.asFloat32Array par)
        Nothing   -> throwError $ ShaderError "missing par uniform!"

    when (length zn > 0) do
      mZnU <- liftEff $ GL.getUniformLocation ctx prog "zn"
      case mZnU of
        Just znU -> uniform2fv (Uniform znU) (T.asFloat32Array znA)
        Nothing  -> throwError $ ShaderError "missing zn uniform!"
  where
    fn (Cartesian r i) = [r, i]


-- execute a webgl action & wrap its error
execGL :: forall eff a. WebGLContext -> WebGL a -> Epi eff a
execGL ctx webGL = do
  res <- lift $ runWebgl webGL ctx
  either (throwError <<< show) return res
