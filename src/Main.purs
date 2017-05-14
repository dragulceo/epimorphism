module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST, STRef, readSTRef, newSTRef, modifySTRef, runST)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import Data.Array (concatMap, null)
import Data.Comp (CompOp(..))
import Data.Int (round, toNumber)
import Data.Kernels (Kernel)
import Data.Library (dat, getLib, getLibM, getPattern, getPatternD, getSystemConf, getSystemConfD, getUIConfD, modLibD, setLib)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Set (toUnfoldable)
import Data.StrMap (lookup)
import Data.System (EngineST, SystemST, UIST, defaultSystemST)
import Data.Types (EngineConf, EpiS, Library(..), Section(..))
import Engine.Compiler (compileShaders)
import Engine.Engine (initEngineST, executeKernels)
import Engine.Texture (loadImages)
import Graphics.Canvas (CANVAS)
import Pattern (importPattern)
import Script.Script (runScripts)
import System (initLibrary)
import UI.Layout (updateLayout)
import UI.UI (initUIST)
import Util (Now, enableDebug, getProfileCookie, glob, handleError, isDev, isHalted, log, now, requestAnimationFrame, rndstr, seedRandom, urlArgs, getVersion)

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
  let def = if dev then "test" else "prod"
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
  pattern <- getPattern lib "importPattern"
  pId     <- importPattern lib pattern Nothing

  -- update system
  modLibD lib systemConf _ {seed = seed, pattern = pId}

  lift $ log "AFTER IMPORT"
  lift $ log lib

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

  --lift $ log $ "EXECUTING FRAME: " <> (show systemST.frameNum)
  --lift $ log lib

  --when (systemST.frameNum == 3) do
  --  lift $ halt

  -- update time
  currentTimeMS <- lift $ now
  let lastTimeMS = fromMaybe currentTimeMS systemST.lastTimeMS

  let delta = 0.01 -- a fixed increment of time looks better (also maybe stick this # in system?)
  let pauseF = if systemST.paused then 0.0 else 1.0
  let t' = systemST.t + pauseF * delta
  lift $ modifySTRef ssRef (\s -> s {t = t', lastTimeMS = Just currentTimeMS})

  -- fps
  when (systemST.frameNum `mod` uiConfD.uiUpdateFreq == 0) do
    let lastFpsTimeMS = fromMaybe currentTimeMS systemST.lastFpsTimeMS
    let fps = round $ (toNumber uiConfD.uiUpdateFreq) * 1000.0 / (currentTimeMS - lastFpsTimeMS)
    lift $ modifySTRef ssRef _ {lastFpsTimeMS = Just currentTimeMS, fps = Just fps}
    pure unit

  t1 <- lift $ now
  -- run scripts if not compiling
  when (null engineST.compQueue) do
    kernels <- runScripts ssRef lib
    let kernels' = toUnfoldable kernels :: Array Kernel
    let queue = concatMap (\k -> [CompShader k, CompProg k]) kernels'
    unless (null queue) do
      lift $ modifySTRef esRef _ {compQueue = queue <> [CompFinish]} # void

  systemST' <- lift $ readSTRef ssRef
  engineST' <- lift $ readSTRef esRef

  t2 <- lift $ now

  -- execute compile queue
  when (not $ null engineST'.compQueue) do
    --t' <- lift $ now
    --lift $ modifySTRef esRef _ {compQueue = fullCompile}
    compileShaders esRef lib false # void
    --t'' <- lift $ now
    --lift $ log $ inj "COMPILE :%0ms" [show (t'' - t')]


  t3 <- lift $ now

  engineST'' <- lift $ readSTRef esRef
  systemST'' <- lift $ readSTRef ssRef
  patternD'  <- getPatternD lib "animate pattern'"

  unless systemST''.paused do
    lift $ glob "library" lib
    executeKernels lib systemST'' engineST'' patternD'

  t4 <- lift $ now

  -- update ui
  updateLayout uiST systemST'' engineST'' lib false
  t5 <- lift $ now


  -- re-pause if just getting a frame
  when systemST''.next do
    lift $ modifySTRef ssRef _ {paused = true, next = false} # void

  -- request next frame
  halted <- lift $ isHalted
  unless halted do
    lift $ modifySTRef ssRef (\s -> s {frameNum = s.frameNum + 1})
    lift $ requestAnimationFrame animate state
  t6 <- lift $ now

  --lift $ log $ inj "BREAKDOWN: init:%0ms scripts:%1ms compile:%2ms render:%3ms ui:%4ms next:%5ms" [show (t1 - t0), show (t2 - t1), show (t3 - t2), show (t4 - t3), show (t5 - t4), show (t6 - t5)]

  pure unit

main :: Eff (canvas :: CANVAS, dom :: DOM, now :: Now) Unit
main = do
  runST do
    handleError do
      version <- lift getVersion
      let systemST = defaultSystemST {version = version}
      lift $ log version
      lib      <- initLibrary host version
      state    <- initState systemST lib
      lift $ animate state
