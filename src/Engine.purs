module Engine where

import Prelude
import Data.Complex(Cartesian(..), inCartesian)
import Data.Tuple
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Either
import Data.Array
import Data.TypedArray as T
import Data.Int (toNumber, fromNumber)
import Data.StrMap (StrMap(..))
import Data.Traversable (traverse)

import Control.Monad (when)
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT (), lift)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.ST

import Graphics.WebGL
import Graphics.WebGL.Types
import Graphics.WebGL.Raw as GL
import Graphics.WebGL.Raw.Enums as GLE
import Graphics.WebGL.Raw.Types as GLT
import Graphics.WebGL.Context
import Graphics.WebGL.Shader
import Graphics.WebGL.Methods
import Graphics.Canvas (Canvas, getCanvasElementById, setCanvasWidth, setCanvasHeight)

import Config
import Compiler
import Util (lg, unsafeNull, winLog)

-- PUBLIC

-- create a texture
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


-- initialize framebuffer/texture combo
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
initAux :: WebGL (Array WebGLTexture)
initAux = do
  let numAux = 10
  traverse (\_ -> getTex) (0..(numAux - 1))


-- upload aux textures
loadAux :: forall eff. EngineST -> String -> Array String -> Epi eff Int
loadAux es host names = do
  when (isNothing es.aux) do
    throwError "aux textures not initialized"

  Just aux <- return es.aux
  when (length aux < length names) do
    throwError "not enough aux textures"

  foldM (createImage es.ctx host) 0 (zip aux names)


-- create an image object. can throw error if images missing!  also some synchronization issues
createImage :: forall eff. WebGLContext -> String -> Int -> (Tuple WebGLTexture String) -> Epi eff Int
createImage ctx host c (Tuple aux name) = do
  lift $ createImageImpl (host ++ name) \img -> do
    runWebgl (do
      liftEff $ GL.bindTexture ctx GLE.texture2d aux
      liftEff $ GL.texImage2D ctx GLE.texture2d 0 GLE.rgba GLE.rgba GLE.unsignedByte img) ctx
    return unit
  return $ c + 1


foreign import createImageImpl :: forall eff. String ->
                                  (GLT.TexImageSource -> Eff eff Unit) ->
                                  Eff eff Unit

-- compile shaders and load into systemST
setShaders :: forall eff h. SystemConf -> STRef h EngineST -> SystemST h -> Pattern -> EpiS eff h Unit
setShaders sysConf esRef sys pattern = do
  let t = lg "setShaders"
  es <- lift $ readSTRef esRef

  -- load & compile shaders
  {main, disp, vert, aux} <- compileShaders pattern sys
  cnt <- loadAux es sysConf.host aux

  Tuple main disp <- execGL es.ctx ( do
    -- create programs
    mainProg <- compileShadersIntoProgram vert main
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

  lift $ modifySTRef esRef (\s -> s {dispProg = Just disp, mainProg = Just main, auxN = cnt})
  return unit


-- initialize the rendering engine & create state
initEngineST :: forall eff h. SystemConf -> EngineConf -> SystemST h -> Pattern -> String -> EpiS eff h (STRef h EngineST)
initEngineST sysConf engineConf sys pattern canvasId = do
  -- find canvas & create context
  canvasM <- liftEff $ getCanvasElementById canvasId
  canvas <- case canvasM of
    Just c -> return c
    Nothing -> throwError $ "init engine - canvas not found: " ++ canvasId

  ctxM <- liftEff $ getWebglContext canvas
  ctx <- case ctxM of
    Just c -> return c
    Nothing -> throwError "Unable to get a webgl context!!!"

  -- default state
  let es = {dispProg: Nothing, mainProg: Nothing, tex: Nothing, fb: Nothing, aux: Nothing, auxN: 0, ctx: ctx}
  esRef <- lift $ newSTRef es

  -- if we change kernel_dim we need to redo this
  let dim = engineConf.kernelDim
  lift $ setCanvasWidth (toNumber dim) canvas
  lift $ setCanvasHeight (toNumber dim) canvas

  -- webgl initialization
  res <- execGL ctx do
    Tuple tex0 fb0 <- initTex dim
    Tuple tex1 fb1 <- initTex dim
    aux <- initAux

    clearColor 0.0 0.0 0.0 1.0
    liftEff $ GL.clear ctx GLE.colorBufferBit
    liftEff $ GL.viewport ctx 0 0 dim dim

    return $ es {
        fb  = Just (Tuple fb0 fb1)
      , tex = Just (Tuple tex0 tex1)
      , aux = Just aux
    }

  -- set shaders
  esRef' <- lift $ newSTRef res
  setShaders sysConf esRef' sys pattern

  return esRef'


-- do the thing!
render :: forall eff h. SystemST h -> EngineConf -> EngineST -> Pattern -> Int -> EpiS eff h Unit
render systemST engineConf engineST pattern frameNum = do
  let ctx = engineST.ctx

  -- unpack
  tex <- case engineST.tex of
    Just x -> return x
    Nothing -> throwError "Render: missing textures"
  fbs <- case engineST.fb of
    Just x -> return x
    Nothing -> throwError "Render: missing framebuffers"
  aux <- case engineST.aux of
    Just x -> return x
    Nothing -> throwError "Render: missing aux"
  main <- case engineST.mainProg of
    Just x -> return x
    Nothing -> throwError "Render: missing main program"
  disp <- case engineST.dispProg of
    Just x -> return x
    Nothing -> throwError "Render: missing disp program"

  -- bind par & zn
  bindParZn systemST.moduleRefPool ctx main pattern.main
  bindParZn systemST.moduleRefPool ctx disp pattern.disp

  execGL ctx do
    -- ping pong buffers
    let tm = if frameNum `mod` 2 == 0 then fst tex else snd tex
    let td = if frameNum `mod` 2 == 1 then fst tex else snd tex
    let fb = if frameNum `mod` 2 == 1 then fst fbs else snd fbs

    -- main program
    liftEff $ GL.useProgram ctx main
    liftEff $ GL.bindTexture ctx GLE.texture2d tm
    liftEff $ GL.bindFramebuffer ctx GLE.framebuffer fb

    -- bind main uniforms
    mainUnif <- getUniformBindings main
    uniform1f mainUnif.time ((pattern.t - pattern.tPhase) / 1000.0) -- ms or seconds here?
    uniform1f mainUnif.kernel_dim (toNumber engineConf.kernelDim)

    -- aux
    when (engineST.auxN > 0) do
      auxU <- liftEff $ GL.getUniformLocation ctx main "aux"
      case auxU of
        Just auxU -> liftEff $ GL.uniform1iv ctx auxU (1..engineST.auxN)
        Nothing   -> throwError $ ShaderError "missing aux uniform!"
      liftEff $ forE 0.0 (toNumber engineST.auxN) \i -> do
        let i' = fromJust $ fromNumber i
        GL.activeTexture ctx (GLE.texture1 + i')
        GL.bindTexture ctx GLE.texture2d $ fromJust (aux !! i')

    -- draw
    liftEff $ GL.activeTexture ctx GLE.texture0
    drawArrays Triangles 0 6

    debug

    -- disp/post program
    liftEff $ GL.useProgram ctx disp
    liftEff $ GL.bindTexture ctx GLE.texture2d td
    liftEff $ GL.bindFramebuffer ctx GLE.framebuffer unsafeNull

    -- bind disp uniforms
    dispUnif <- getUniformBindings disp
    uniform1f dispUnif.kernel_dim (toNumber engineConf.kernelDim)

    -- draw
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
