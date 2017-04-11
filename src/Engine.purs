module Engine where

import Prelude
import Data.TypedArray as T
import Graphics.WebGL.Raw as GL
import Graphics.WebGL.Raw.Enums as GLE
import Audio (audioData, initAudio)
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (lift)
import Control.Monad.ST (writeSTRef, STRef, newSTRef, modifySTRef, readSTRef)
import Data.Array (length, (..))
import Data.Int (toNumber)
import Data.Library (getEngineConfD)
import Data.Maybe (maybe, Maybe(Nothing, Just))
import Data.Tuple (Tuple(Tuple), snd, fst)
import Data.Types (EpiS, Epi, Library, EngineProfile, EngineST, SystemST, UniformBindings, fullCompile, newCompST)
import EngineUtil (execGL)
import Graphics.Canvas (setCanvasHeight, setCanvasWidth, getCanvasElementById)
import Graphics.WebGL.Context (getWebglContextWithAttrs, defaultWebglContextAttrs)
import Graphics.WebGL.Methods (uniform2fv, uniform1fv, drawArrays, uniform1f, clearColor)
import Graphics.WebGL.Raw (getParameter)
import Graphics.WebGL.Types (WebGLContext, WebGLTexture, DrawMode(Triangles), Uniform(Uniform), WebGLError(ShaderError))
import Texture (initAuxTex, initTexFb, emptyImage)
import Util (Now, dbg, dbg2, fromJustE, hasAttr, unsafeGetAttr, unsafeNull, zipI)

--  PUBLIC

foreign import getOS :: forall eff. Eff eff String
foreign import getBrowser :: forall eff. Eff eff String
foreign import getIsMobile :: forall eff. Eff eff Boolean
foreign import getAngle :: forall eff. WebGLContext -> Eff eff Boolean

getEngineProfile :: forall eff. WebGLContext -> Epi eff EngineProfile
getEngineProfile ctx = do
  os        <- lift $ getOS
  browser   <- lift $ getBrowser
  is_mobile <- lift $ getIsMobile
  angle     <- lift $ getAngle ctx
  case angle of
    true -> dbg2 "ANGLE Detected"
    false -> pure unit

  max_texture_units' <- liftEff $ getParameter ctx GLE.maxTextureImageUnits
  max_texture_size'  <- liftEff $ getParameter ctx GLE.maxTextureSize
  max_frag_uniforms' <- liftEff $ getParameter ctx GLE.maxFragmentUniformVectors
  max_texture_units  <- fromJustE max_texture_units' "invalid param"
  max_texture_size   <- fromJustE max_texture_size'  "invalid param"
  max_frag_uniforms  <- fromJustE max_frag_uniforms' "invalid param"

  pure $ { os, browser, is_mobile, angle, max_texture_units, max_texture_size, max_frag_uniforms }


-- initialize the rendering engine & create state.  updates an existing state if passed
-- maybe validate that kernelDim > 0?
initEngineST :: forall eff h. Library h -> String -> Maybe (STRef h EngineST) -> EpiS (now :: Now | eff) h (STRef h EngineST)
initEngineST lib canvasId esRef' = do
  engineConfD <- getEngineConfD lib "compileShaders"

  -- find canvas & create context
  canvasM <- liftEff $ getCanvasElementById canvasId
  canvas <-
    case canvasM of
      Just c -> pure c
      Nothing -> throwError $ "init engine - canvas not found: " <> canvasId

  let attrs = defaultWebglContextAttrs {
        alpha =                 false
        , depth =                 false
        , antialias =             false
        , preserveDrawingBuffer = true}

  ctxM <- liftEff $ getWebglContextWithAttrs canvas attrs
  ctx <- case ctxM of
    Just c -> pure c
    Nothing -> throwError "Unable to get a webgl context!!!"

  profile <- getEngineProfile ctx
  dbg profile
  --liftEff $ getExtension ctx "OES_texture_float"
  --liftEff $ getExtension ctx "OES_texture_float_linear"

  empty <- lift $ emptyImage engineConfD.kernelDim

  -- get reference
  esRef <- case esRef' of
    Just ref -> do
      lift $ modifySTRef ref (\r -> r {empty = empty, currentImages = Nothing})
      pure ref
    Nothing -> do
      let compST = newCompST
      let tmp = {dispProg: Nothing, mainProg: Nothing, tex: Nothing, fb: Nothing, auxTex: Nothing, audio: Nothing, currentImages: Nothing, compQueue: fullCompile, mainUnif: Nothing, dispUnif: Nothing, ctx, empty, compST, profile}
      lift $ newSTRef tmp

  es <- lift $ readSTRef esRef

  -- if we change kernel_dim we need to redo this
  let dim = engineConfD.kernelDim
  lift $ setCanvasWidth (toNumber dim) canvas
  lift $ setCanvasHeight (toNumber dim) canvas

  -- webgl initialization
  res <- execGL ctx do
    Tuple tex0 fb0 <- initTexFb dim
    Tuple tex1 fb1 <- initTexFb dim
    auxTex <- initAuxTex engineConfD es.ctx empty
    audio <- initAudio engineConfD es.ctx empty

    clearColor 0.0 0.0 0.0 1.0
    liftEff $ GL.clear ctx GLE.colorBufferBit
    liftEff $ GL.viewport ctx 0 0 dim dim

    pure $ es {
        fb  = Just (Tuple fb0 fb1)
      , tex = Just (Tuple tex0 tex1)
      , auxTex = Just auxTex
      , audio = audio
    }

  -- set shaders
  lift $ writeSTRef esRef res

  pure esRef

-- do the thing!
renderFrame :: forall eff h. SystemST h -> EngineST -> Library h -> Array Number -> Array Number -> Int -> EpiS eff h WebGLTexture
renderFrame systemST engineST lib par zn frameNum = do
  let ctx = engineST.ctx
  engineConfD <- getEngineConfD lib "postprocessFrams"

  -- unpack
  tex <- case engineST.tex of
    Just x -> pure x
    Nothing -> throwError "RenderFrame: missing textures"
  fbs <- case engineST.fb of
    Just x -> pure x
    Nothing -> throwError "RenderFrame: missing framebuffers"
  auxTex <- case engineST.auxTex of
    Just x -> pure x
    Nothing -> throwError "RenderFrame: missing auxTex"
  main <- case engineST.mainProg of
    Just x -> pure x
    Nothing -> throwError "RenderFrame: missing main program"
  mainUnif <- case engineST.mainUnif of
    Just x -> pure x
    Nothing -> throwError "RenderFrame: missing main program"

  -- bind par & zn
  execGL ctx (liftEff $ GL.useProgram ctx main)
  bindParZn ctx mainUnif par zn

  execGL ctx do
    -- bind main uniforms
    uniform1f (unsafeGetAttr mainUnif "time") systemST.t -- - pattern.tPhase
    uniform1f (unsafeGetAttr mainUnif "kernel_dim") (toNumber engineConfD.kernelDim)

    -- BUG!!! audio has to be before aux???
    --audio info

    let numAux = maybe 0 length engineST.currentImages
    case engineST.audio of
      Just (Tuple audioTex analyser) -> do
        case (hasAttr mainUnif "audioData") of
          true -> do
            liftEff $ GL.bindTexture ctx GLE.texture2d audioTex
            dta <- lift $ lift $ audioData analyser
            liftEff $ GL.texImage2D_ ctx GLE.texture2d 0 GLE.alpha engineConfD.audioBufferSize 1 0 GLE.alpha GLE.unsignedByte dta

            let ofs = numAux + 1
            liftEff $ GL.uniform1i ctx (unsafeGetAttr mainUnif "audioData") ofs
            liftEff $ GL.activeTexture ctx (GLE.texture0 + ofs)
            liftEff $ GL.bindTexture ctx GLE.texture2d audioTex
          false -> pure unit
      Nothing -> pure unit

    -- aux
    when (numAux > 0) do
      when (not $ hasAttr mainUnif "aux[0]") do
        throwError $ ShaderError "missing aux uniform!"
      liftEff $ GL.uniform1iv ctx (unsafeGetAttr mainUnif "aux[0]") (1..numAux)
      liftEff $ foreachE (zipI auxTex) \(Tuple i t) -> do
        GL.activeTexture ctx (GLE.texture1 + i)
        GL.bindTexture ctx GLE.texture2d t


    -- ping pong buffers
    let tm = if frameNum `mod` 2 == 0 then fst tex else snd tex
    let td = if frameNum `mod` 2 == 1 then fst tex else snd tex
    let fb = if frameNum `mod` 2 == 1 then fst fbs else snd fbs

    -- draw
    liftEff $ GL.activeTexture ctx GLE.texture0
    liftEff $ GL.bindTexture ctx GLE.texture2d tm
    liftEff $ GL.bindFramebuffer ctx GLE.framebuffer fb
    drawArrays Triangles 0 6

    pure td


postprocessFrame :: forall eff h. SystemST h -> EngineST -> Library h -> WebGLTexture -> Array Number -> Array Number -> EpiS eff h Unit
postprocessFrame systemST engineST lib tex par zn = do
  let ctx = engineST.ctx
  engineConfD <- getEngineConfD lib "postprocessFrams"

  disp <- case engineST.dispProg of
    Just x -> pure x
    Nothing -> throwError "RenderFrame: missing disp program"
  dispUnif <- case engineST.dispUnif of
    Just x -> pure x
    Nothing -> throwError "RenderFrame: missing disp program"

  execGL ctx (liftEff $ GL.useProgram ctx disp)
  bindParZn ctx dispUnif par zn

  execGL ctx do
    -- bind disp uniforms
    uniform1f (unsafeGetAttr dispUnif "kernel_dim") (toNumber engineConfD.kernelDim)

    -- draw
    liftEff $ GL.bindTexture ctx GLE.texture2d tex
    liftEff $ GL.bindFramebuffer ctx GLE.framebuffer unsafeNull
    drawArrays Triangles 0 6


-- bind parameters & zn values from pattern into program
bindParZn :: forall h eff. WebGLContext -> UniformBindings -> Array Number -> Array Number -> EpiS eff h Unit
bindParZn ctx unif par zn = do
  execGL ctx do
    when (length par > 0) do
      when (not $ hasAttr unif "par[0]") do
        throwError $ ShaderError "missing par binding!"
      uniform1fv (Uniform (unsafeGetAttr unif "par[0]")) (T.asFloat32Array par)

    when (length zn > 0) do
      when (not $ hasAttr unif "zn[0]") do
        throwError $ ShaderError "missing zn binding!"
      uniform2fv (Uniform (unsafeGetAttr unif "zn[0]")) (T.asFloat32Array zn)
