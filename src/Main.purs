module Main where

import Prelude
import Compiler (compileShaders)
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (ST, STRef, readSTRef, newSTRef, modifySTRef, runST)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import Data.Array (null, updateAt, foldM, sort, concatMap, fromFoldable)
import Data.Int (round, toNumber)
import Data.Library (dat, getLib, getLibM, getPatternD, getSystemConf, getSystemConfD, getUIConfD, modLibD)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe, isNothing)
import Data.Set (member)
import Data.StrMap (insert, values, keys, lookup)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Types (CompOp(..), Component(..), EngineConf, EngineST, EpiS, Library(..), Module(..), PMut(PMutNone, PMut), Section(..), SystemST, UIST, defaultSystemST, fullCompile)
import Engine (postprocessFrame, initEngineST, renderFrame)
import Graphics.Canvas (CANVAS)
import Layout (updateLayout)
import Paths (runPath)
import Pattern (importPattern)
import Script (runScripts)
import System (initLibrary)
import Texture (loadImages)
import UI (initUIST)
import Util (Now, log, enableDebug, fromJustE, getProfileCookie, handleError, imag, inj, isDev, isHalted, now, real, requestAnimationFrame, rndstr, seedRandom, urlArgs, zipI)

host :: String
host = ""

type State h = {
    usRef :: STRef h UIST
  , ssRef :: STRef h (SystemST h)
  , esRef :: STRef h EngineST
  , lib   :: Library h
}


getSysConfName :: forall eff. Eff eff String
getSysConfName = do
  args <- urlArgs
  dev <- isDev
  let def = if dev then "dev" else "prod"
  let conf = fromMaybe def (lookup "system" args)
  pure conf


initState :: forall eff h. SystemST h -> Library h -> EpiS (now :: Now | eff) h (State h)
initState systemST lib'@(Library libVar) = do
  --  init config & system
  systemName <- lift $ getSysConfName
  let lib = Library libVar{system = Just systemName}
  lift $ log lib

  systemConf <- getSystemConf lib "init system"
  let systemConfD = dat systemConf
  when systemConfD.debug do
    lift $ enableDebug

  -- set random seed
  seed <- case systemConfD.seed of
    "" -> do
      newSeed <- lift rndstr
      lift $ log $ "GENERATING SEED: " <> newSeed
      pure newSeed
    _ -> do
      lift $ log $ "USING SEED: " <> systemConfD.seed
      pure systemConfD.seed

  modLibD lib systemConf _ {seed = seed}
  lift $ seedRandom seed

  -- if the cookie_profile sets a valid profile, update the system
  cookie_profile <- lift $ getProfileCookie
  elt <- getLibM lib cookie_profile
  case (elt :: Maybe EngineConf) of
    (Just d) -> modLibD lib systemConf _ {engineConf = cookie_profile}
    Nothing -> do
      lift $ log $ "Unknown EngineConf: " <> cookie_profile  <> " - using default instead"
      pure unit

  uiConfD <- getUIConfD lib "init system"
  patternD <- getPatternD lib "init pattern"

  -- build strefs
  ssRef <- lift $ newSTRef systemST

  -- load image libraries
  (Section _ index') <- getLib lib patternD.defaultImageLib "can't find default image lib!"
  (Section _ index)  <- getLib lib patternD.imageLib "can't find image lib!"

  lift $ loadImages index'.lib index.lib

  -- import pattern
  importPattern lib

  -- init engine & ui states
  esRef <- initEngineST lib uiConfD.canvasId Nothing
  usRef <- initUIST esRef ssRef lib

  pure {usRef, ssRef, esRef, lib}


animate :: forall h. (State h) -> Eff (canvas :: CANVAS, dom :: DOM, now :: Now, st :: ST h) Unit
animate state = handleError do
  t0 <- lift $ now
  -- unpack state
  {usRef, ssRef, esRef, lib} <- pure state

  systemConfD <- getSystemConfD lib "animate systemConf"
  uiConfD     <- getUIConfD lib "animate uiConf"
  patternD    <- getPatternD lib "animate pattern"
  --lift $ log patternD.main

  uiST       <- lift $ readSTRef usRef
  systemST   <- lift $ readSTRef ssRef
  engineST   <- lift $ readSTRef esRef

  -- update time
  currentTimeMS <- lift $ now
  let lastTimeMS = fromMaybe currentTimeMS systemST.lastTimeMS

  let delta = 0.02 -- a fixed increment of time looks better (also maybe stick this # in system?
  --let delta = (currentTimeMS - lastTimeMS) * pattern.tSpd / 1000.0
  let pauseF = if systemST.paused then 0.0 else 1.0
  let t' = systemST.t + pauseF * delta
  lift $ modifySTRef ssRef (\s -> s {t = t', lastTimeMS = Just currentTimeMS})

  -- fps
  when (systemST.frameNum `mod` uiConfD.uiUpdateFreq == 0) do
    let lastFpsTimeMS = fromMaybe currentTimeMS systemST.lastFpsTimeMS
    let fps = round $ (toNumber uiConfD.uiUpdateFreq) * 1000.0 / (currentTimeMS - lastFpsTimeMS)
    lift $ modifySTRef ssRef (\s -> s {lastFpsTimeMS = Just currentTimeMS, fps = Just fps})
    pure unit

  t1 <- lift $ now
  -- run scripts if not compiling
  when (null engineST.compQueue) do
    sRes <- runScripts ssRef lib
    case sRes of
      PMutNone -> pure unit
      PMut _ new -> do
        let queue = (if (member "main" new) then [CompMainShader, CompMainProg] else []) <>
                    (if (member "disp" new) then [CompDispShader, CompDispProg] else []) <>
                    [CompFinish]

        lift $ modifySTRef esRef (\es -> es {compQueue = queue})
        pure unit

  systemST' <- lift $ readSTRef ssRef
  engineST' <- lift $ readSTRef esRef

  -- execute compile queue
  when (not $ null engineST'.compQueue) do
    --t' <- lift $ now
    --lift $ modifySTRef esRef _ {compQueue = fullCompile}
    compileShaders esRef lib (isNothing engineST.mainProg)
    --t'' <- lift $ now
    --lift $ log $ inj "COMPILE :%0ms" [show (t'' - t')]
    --currentTimeMS2 <- lift $ now
    --lift $ modifySTRef ssRef (\s -> s {lastTimeMS = Just currentTimeMS2})
    pure unit

  t2 <- lift $ now

  engineST'' <- lift $ readSTRef esRef
  systemST'' <- lift $ readSTRef ssRef
  patternD' <- getPatternD lib "animate pattern'"

  -- render!
  t3 <- lift $ now
  (Tuple parM znM) <- getParZn lib t' (Tuple [] []) patternD'.main
  tex <- renderFrame systemST'' engineST'' lib parM znM systemST''.frameNum

  (Tuple parD znD) <- getParZn lib t' (Tuple [] []) patternD'.disp
  postprocessFrame systemST'' engineST'' lib tex parD znD
  t4 <- lift $ now

  -- update ui
  updateLayout uiST systemST'' engineST'' lib false
  t5 <- lift $ now

  -- request next frame
  halted <- lift $ isHalted
  unless halted do
    lift $ modifySTRef ssRef (\s -> s {frameNum = s.frameNum + 1})
    lift $ requestAnimationFrame animate state
  t6 <- lift $ now

  --lift $ log $ inj "BREAKDOWN: init:%0ms scripts:%1ms recompile:%2ms render:%3ms ui:%4ms next:%5ms" [show (t1 - t0), show (t2 - t1), show (t3 - t2), show (t4 - t3), show (t5 - t4), show (t6 - t5)]

  pure unit

-- recursively flatten par & zn lists in compilation order
getParZn :: forall eff h. Library h -> Number -> (Tuple (Array Number) (Array Number)) -> String -> EpiS eff h (Tuple (Array Number) (Array Number))
getParZn lib t (Tuple par zn) mid = do
  mod@(Module _ modD) <- getLib lib mid "mid getParZn"

  znV <- traverse (runZnPath mid t) (zipI modD.zn)
  let znV' = concatMap (\x -> [real x, imag x]) znV
  let zn' = zn <> znV'

  parV <- traverse (runParPath mid t) (sort $ keys modD.par)
  let par' = par <> parV

  foldM (getParZn lib t) (Tuple par' zn') (fromFoldable $ values modD.modules)
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

main :: Eff (canvas :: CANVAS, dom :: DOM, now :: Now) Unit
main = do
  runST do
    handleError do
      let systemST = defaultSystemST
      lib      <- initLibrary host
      state    <- initState systemST lib
      lift $ animate state
