module Engine where

import Prelude
import Data.TypedArray as T
import Graphics.WebGL.Raw as GL
import Graphics.WebGL.Raw.Enums as GLE
import Audio (audioData, initAudio)
import Compiler (compileShaders)
import Config (CompST(CompDone), EpiS, Pattern, EngineST, EngineConf, SystemST, SystemConf)
import Control.Monad (when)
import Control.Monad.Eff (forE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (lift)
import Control.Monad.ST (writeSTRef, STRef, newSTRef, modifySTRef, readSTRef)
import Data.Array (length, (!!), (..))
import Data.Int (toNumber, fromNumber)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Maybe.Unsafe (fromJust)
import Data.Tuple (Tuple(Tuple), snd, fst)
import EngineUtil (execGL)
import Graphics.Canvas (setCanvasHeight, setCanvasWidth, getCanvasElementById)
import Graphics.WebGL.Context (getWebglContextWithAttrs, defaultWebglContextAttrs)
import Graphics.WebGL.Methods (uniform2fv, uniform1fv, drawArrays, uniform1f, clearColor)
import Graphics.WebGL.Shader (getUniformBindings)
import Graphics.WebGL.Types (WebGLProgram, WebGLContext, WebGLTexture, DrawMode(Triangles), Uniform(Uniform), WebGLError(ShaderError))
import Texture (initAux, initTexFb, emptyImage)
import Util (dbg, Now, unsafeNull)

-- PUBLIC

-- initialize the rendering engine & create state.  updates an existing state if passed
-- maybe validate that kernelDim > 0?
initEngineST :: forall eff h. SystemConf -> EngineConf -> SystemST h -> Pattern -> String -> Maybe (STRef h EngineST) -> EpiS (now :: Now | eff) h (STRef h EngineST)
initEngineST sysConf engineConf systemST pattern canvasId esRef' = do
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
      lift $ modifySTRef ref (\r -> r {empty = empty, auxImg = []})
      return ref
    Nothing -> do
      let tmp = {dispProg: Nothing, mainProg: Nothing, tex: Nothing, fb: Nothing, aux: Nothing, audio: Nothing, auxImg: [], compST: CompDone, ctx, empty}
      lift $ newSTRef tmp

  es <- lift $ readSTRef esRef

  -- if we change kernel_dim we need to redo this
  let dim = engineConf.kernelDim
  lift $ setCanvasWidth (toNumber dim) canvas
  lift $ setCanvasHeight (toNumber dim) canvas

  -- webgl initialization
  res <- execGL ctx do
    Tuple tex0 fb0 <- initTexFb dim
    Tuple tex1 fb1 <- initTexFb dim
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
  compileShaders sysConf systemST engineConf esRef pattern

  return esRef

-- do the thing!
renderFrame :: forall eff h. SystemST h -> EngineConf -> EngineST -> Pattern -> Array Number -> Array Number -> Int -> EpiS eff h WebGLTexture
renderFrame systemST engineConf engineST pattern par zn frameNum = do
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

  -- bind par & zn
  bindParZn ctx main par zn

  execGL ctx do
    liftEff $ GL.useProgram ctx main

    -- bind main uniforms
    mainUnif <- getUniformBindings main
    uniform1f mainUnif.time (systemST.t - pattern.tPhase)
    uniform1f mainUnif.kernel_dim (toNumber engineConf.kernelDim)

    -- BUG!!! audio has to be before aux???
    --audio info
    case engineST.audio of
      Just (Tuple audioTex analyser) -> do
        audioU <- liftEff $ GL.getUniformLocation ctx main "audioData"
        case audioU of
          Just audioU' -> do
            liftEff $ GL.bindTexture ctx GLE.texture2d audioTex
            dta <- lift $ lift $ audioData analyser
            liftEff $ GL.texImage2D_ ctx GLE.texture2d 0 GLE.alpha engineConf.audioBufferSize 1 0 GLE.alpha GLE.unsignedByte dta

            let ofs = length engineST.auxImg + 1
            liftEff $ GL.uniform1i ctx audioU' ofs
            liftEff $ GL.activeTexture ctx (GLE.texture0 + ofs)
            liftEff $ GL.bindTexture ctx GLE.texture2d audioTex
          Nothing -> return unit
      Nothing -> return unit

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

    -- ping pong buffers
    let tm = if frameNum `mod` 2 == 0 then fst tex else snd tex
    let td = if frameNum `mod` 2 == 1 then fst tex else snd tex
    let fb = if frameNum `mod` 2 == 1 then fst fbs else snd fbs

    -- draw
    liftEff $ GL.activeTexture ctx GLE.texture0
    liftEff $ GL.bindTexture ctx GLE.texture2d tm
    liftEff $ GL.bindFramebuffer ctx GLE.framebuffer fb
    drawArrays Triangles 0 6

    return td


postprocessFrame :: forall eff h. SystemST h -> EngineConf -> EngineST -> WebGLTexture -> Array Number -> Array Number -> EpiS eff h Unit
postprocessFrame systemST engineConf engineST tex par zn = do
  let ctx = engineST.ctx

  disp <- case engineST.dispProg of
    Just x -> return x
    Nothing -> throwError "RenderFrame: missing disp program"

  bindParZn ctx disp par zn

  execGL ctx do
    -- disp/post program
    liftEff $ GL.useProgram ctx disp

    -- bind disp uniforms
    dispUnif <- getUniformBindings disp
    uniform1f dispUnif.kernel_dim (toNumber engineConf.kernelDim)

    -- draw
    liftEff $ GL.bindTexture ctx GLE.texture2d tex
    liftEff $ GL.bindFramebuffer ctx GLE.framebuffer unsafeNull
    drawArrays Triangles 0 6

-- bind parameters & zn values from pattern into program
bindParZn :: forall h eff. WebGLContext -> WebGLProgram -> Array Number -> Array Number -> EpiS eff h Unit
bindParZn ctx prog par zn = do
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
        Just znU -> uniform2fv (Uniform znU) (T.asFloat32Array zn)
        Nothing  -> throwError $ ShaderError "missing zn uniform!"
