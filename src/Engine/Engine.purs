module Engine.Engine where

import Prelude
import Data.TypedArray as T
import Graphics.WebGL.Raw as GL
import Graphics.WebGL.Raw.Enums as GLE
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (lift)
import Control.Monad.ST (writeSTRef, STRef, newSTRef, modifySTRef, readSTRef)
import Data.Array (concatMap, elemIndex, foldM, length, sort, updateAt)
import Data.Array (fromFoldable) as A
import Data.Comp (UniformBindings, fullCompile, newCompST)
import Data.Int (toNumber)
import Data.Kernels (Kernel(..), kGet)
import Data.Library (getEngineConfD, getLib, modLibD)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.StrMap (StrMap, empty, fromFoldable, insert, keys, lookup, size, toUnfoldable, values)
import Data.System (EngineProfile, EngineST, SystemST)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.Types (Epi, EpiS, Library, Module(..), PatternD)
import Engine.Audio (initAudio)
import Engine.Compiler (compileShaders)
import Engine.EngineUtil (execGL)
import Engine.Texture (initAuxTex, initTexFb, emptyImage)
import Graphics.Canvas (setCanvasHeight, setCanvasWidth, getCanvasElementById)
import Graphics.WebGL.Context (getWebglContextWithAttrs, defaultWebglContextAttrs)
import Graphics.WebGL.Methods (uniform2fv, uniform1fv, drawArrays, uniform1f, clearColor)
import Graphics.WebGL.Raw (getParameter, getExtension)
import Graphics.WebGL.Types (DrawMode(Triangles), Uniform(Uniform), WebGLContext, WebGLError(ShaderError), WebGLFramebuffer, WebGLTexture)
import Paths (runPath)
import Util (Now, fromJustE, hasAttr, imag, log, real, unsafeGetAttr, unsafeNull, zipI)

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
    true -> lift $ log "ANGLE Detected"
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
  canvas  <- fromJustE canvasM $ "init engine - canvas not found: " <> canvasId

  let dim = engineConfD.kernelDim
  lift $ setCanvasWidth (toNumber dim) canvas
  lift $ setCanvasHeight (toNumber dim) canvas

  let attrs = defaultWebglContextAttrs {
          alpha =                 false
        , depth =                 false
        , antialias =             false
        , preserveDrawingBuffer = true}

  ctxM <- liftEff $ getWebglContextWithAttrs canvas attrs
  ctx <- fromJustE ctxM "Unable to get a webgl context!!!"

  profile <- getEngineProfile ctx
  lift $ log profile
  --liftEff $ getExtension ctx "OES_texture_float"
  --liftEff $ getExtension ctx "OES_texture_float_linear"

  empty <- lift $ emptyImage engineConfD.kernelDim

  -- get reference
  esRef <- case esRef' of
    Just ref -> do
      let compST = newCompST
      let curST  = newCompST
      lift $ modifySTRef ref _ {empty = empty, currentImages = [], curST = curST, compST=compST, compQueue = fullCompile}
      pure ref
    Nothing -> do
      let compST = newCompST
      let curST  = newCompST
      let new = {fb: Nothing, auxTex: Nothing, audio: Nothing, currentImages: [], compQueue: fullCompile, seed: Nothing, ctx, empty, compST, curST, profile}
      lift $ newSTRef new

  es <- lift $ readSTRef esRef

  -- webgl initialization
  ref <- execGL ctx do
--    liftEff $ getExtension ctx "OES_texture_float"

    Tuple tex0 fb0 <- initTexFb dim
    Tuple tex1 fb1 <- initTexFb dim
    auxTex <- initAuxTex engineConfD es.ctx empty
    audio <- initAudio engineConfD es.ctx empty

    seed <- initTexFb dim

    clearColor 0.0 0.0 0.0 1.0
    liftEff $ GL.clear ctx GLE.colorBufferBit
    liftEff $ GL.viewport ctx 0 0 dim dim
    pure $ es {
        fb  = Just (Tuple (Tuple tex0 fb0) (Tuple tex1 fb1))
      , seed = Just seed
      , auxTex = Just auxTex
      , audio = audio
    }

  -- update state
  lift $ writeSTRef esRef ref

  lift $ log "COMPILE INITIAL SHADERS"
  compileShaders esRef lib true
  lift $ log "FINISH COMPILE INITIAL SHADERS"

  pure esRef


executeKernels :: forall eff h. Library h -> SystemST h -> EngineST -> PatternD -> EpiS eff h Unit
executeKernels lib systemST engineST patternD = do
  fbs       <- fromJustE engineST.fb       "executeKernels: missing framebuffers"
  seedTexFb <- fromJustE engineST.seed     "executeKernels: missing seed buffer"

  -- execute seed
  executeKernel lib systemST engineST Seed patternD.seed (snd seedTexFb) empty

  -- ping-pong buffers
  let tex = if systemST.frameNum `mod` 2 == 0 then fst $ fst fbs else fst $ snd fbs
  let fb = if systemST.frameNum `mod` 2 == 1 then snd $ fst fbs else snd $ snd fbs

  -- execute main
  let buffers = fromFoldable [Tuple "fb" tex, Tuple "seedBuf" (fst seedTexFb)]
  executeKernel lib systemST engineST Main patternD.main fb buffers

  -- execute disp
  let buffers' = fromFoldable [Tuple "fb" tex]
  executeKernel lib systemST engineST Disp patternD.disp unsafeNull buffers'


executeKernel :: forall eff h. Library h -> SystemST h -> EngineST -> Kernel -> String ->
                 WebGLFramebuffer -> (StrMap WebGLTexture) -> EpiS eff h Unit
executeKernel lib systemST engineST kernel mid out_fb in_tex = do
  engineConfD <- getEngineConfD lib "kernel ec"
  prog <- fromJustE (kGet engineST.curST.prog kernel) $ "missing program: " <> (show kernel)
  unif <- fromJustE (kGet engineST.curST.unif kernel) $ "missing uniforms: " <> (show kernel)

  -- aux data
  auxTex      <- fromJustE engineST.auxTex   "executeKernel: missing auxTex"
  let auxImages = fromMaybe [] (kGet engineST.curST.aux kernel)
  auxIndexes <- getAuxIndexes auxImages engineST.currentImages

  -- use prog
  let ctx = engineST.ctx
  execGL ctx (liftEff $ GL.useProgram ctx prog)

  lift $ log $ show kernel
  lift $ log $ "numaux"
  lift $ log $ length auxImages
  lift $ log $ size in_tex

  -- bind par & zn
  (Tuple par zn) <- getParZn lib systemST.t (Tuple [] []) mid
  bindParZn ctx unif par zn

  execGL ctx do
    -- bind uniforms
    uniform1f (unsafeGetAttr unif "time") systemST.t
    uniform1f (unsafeGetAttr unif "kernel_dim") (toNumber engineConfD.kernelDim) -- variable dims

    -- bind aux
    let numAux = length auxImages
    when (numAux > 0) do
      when (not $ hasAttr unif "aux[0]") do
        throwError $ ShaderError "missing aux uniform!"
      liftEff $ GL.uniform1iv ctx (unsafeGetAttr unif "aux[0]") auxIndexes
      liftEff $ foreachE (zipI auxTex) \(Tuple i t) -> do
        GL.activeTexture ctx (GLE.texture0 + i)
        GL.bindTexture ctx GLE.texture2d t

    -- bind input buffers
    let indexed = map (\(Tuple i (Tuple var tex)) -> {i, var, tex}) $ zipI (toUnfoldable in_tex)
    for indexed \{i, var, tex} -> do
      liftEff $ GL.activeTexture ctx (GLE.texture0 + numAux + i)
      liftEff $ GL.bindTexture ctx GLE.texture2d tex
      liftEff $ GL.uniform1i ctx (unsafeGetAttr unif var) (i + numAux)

    -- draw to fb
    liftEff $ GL.bindFramebuffer ctx GLE.framebuffer out_fb
    drawArrays Triangles 0 6


    -- bind audio textures
--    case engineST.audio of
--      Just (Tuple audioTex analyser) -> do
--        case (hasAttr mainUnif "audioData") of
--          true -> do
--            liftEff $ GL.bindTexture ctx GLE.texture2d audioTex
--            dta <- lift $ lift $ audioData analyser
--            liftEff $ GL.texImage2D_ ctx GLE.texture2d 0 GLE.alpha engineConfD.audioBufferSize 1 0 GLE.alpha GLE.unsignedByte dta
--
--            let ofs = numAux + 1
--            liftEff $ GL.uniform1i ctx (unsafeGetAttr mainUnif "audioData") ofs
--            liftEff $ GL.activeTexture ctx (GLE.texture0 + ofs)
--            liftEff $ GL.bindTexture ctx GLE.texture2d audioTex
--          false -> pure unit
--      Nothing -> pure unit


getAuxIndexes :: forall eff h. Array String -> Array String -> EpiS eff h (Array Int)
getAuxIndexes images allImages = do
  for images \img -> do
    case (elemIndex img allImages) of
      Just i -> pure i
      Nothing -> throwError $ img <> " not found in currentImages"

-- recursively flatten par & zn lists in compilation order
getParZn :: forall eff h. Library h -> Number -> (Tuple (Array Number) (Array Number)) -> String -> EpiS eff h (Tuple (Array Number) (Array Number))
getParZn lib t (Tuple par zn) mid = do
  mod@(Module _ modD) <- getLib lib mid "mid getParZn"

  znV <- traverse (runZnPath mid t) (zipI modD.zn)
  let znV' = concatMap (\x -> [real x, imag x]) znV

  parV <- traverse (runParPath mid t) (sort $ keys modD.par)

  foldM (getParZn lib t) (Tuple (par <> parV) (zn <> znV')) (A.fromFoldable $ values modD.modules)
  where
    runZnPath mid' t' (Tuple idx val) = do
      (Tuple res remove) <- runPath t' val
      when remove do -- replace with constant
        mod@(Module _ modD) <- getLib lib mid' "mid runZnPath"

        zn' <- fromJustE (updateAt idx (show res) modD.zn) "should be safe getParZn"
        modLibD lib mod _ {zn = zn'}
      pure res
    runParPath mid' t' key = do
      mod@(Module _ modD) <- getLib lib mid' "mid runParPath"

      val <- fromJustE (lookup key modD.par) "cant find val getParZn"
      (Tuple res remove) <- runPath t' val
      let res' = real res
      when remove do -- replace with constant
        let par' = insert key (show res') modD.par
        modLibD lib mod _ {par = par'}
      pure res'


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
