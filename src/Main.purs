module Main where

import Prelude
import Config (UIST, EpiS, Pattern, EngineST, EngineConf, SystemST, SystemConf, UIConf)
import Control.Monad (when)
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (ST, STRef, readSTRef, newSTRef, modifySTRef, runST)
import DOM (DOM)
import Data.Int (round, toNumber)
import Data.Maybe (fromMaybe, Maybe(Just))
import Engine (initEngineST, renderFrame, setShaders)
import Graphics.Canvas (Canvas)
import Layout (updateLayout)
import Pattern (importPattern)
import Script (runScripts)
import System (initSystemST, loadLib)
import UI (initUIST)
import Util (lg, requestAnimationFrame, now, Now, handleError)

host :: String
host = ""

type State h = {
    ucRef :: STRef h UIConf
  , usRef :: STRef h UIST
  , scRef :: STRef h SystemConf
  , ssRef :: STRef h (SystemST h)
  , ecRef :: STRef h EngineConf
  , esRef :: STRef h EngineST
  , pRef  :: STRef h Pattern
}

init :: forall eff h. EpiS eff h (State h)
init = do
  -- init system
  systemST <- initSystemST host
  ssRef <- lift $ newSTRef systemST

  -- init config
  systemConf <- loadLib "default" systemST.systemConfLib "init system"
  let systemConf' = systemConf {host = host}

  engineConf <- loadLib systemConf'.initEngineConf systemST.engineConfLib "init engine"
  uiConf     <- loadLib systemConf'.initUIConf systemST.uiConfLib "init ui"
  pattern    <- loadLib systemConf'.initPattern systemST.patternLib "init pattern"

  -- build strefs
  scRef <- lift $ newSTRef systemConf'
  ecRef <- lift $ newSTRef engineConf
  ucRef <- lift $ newSTRef uiConf
  pRef  <- lift $ newSTRef pattern

  -- import pattern
  importPattern ssRef pRef
  pattern'  <- lift $ readSTRef pRef
  systemST' <- lift $ readSTRef ssRef

  -- init engine & ui states
  esRef <- initEngineST systemConf' engineConf systemST' pattern' uiConf.canvasId
  usRef <- initUIST ucRef ecRef esRef pRef scRef ssRef

  return {ucRef, usRef, ssRef, scRef, ecRef, esRef, pRef}


animate :: forall h. EpiS (now :: Now) h (State h) -> Eff (canvas :: Canvas, dom :: DOM, now :: Now, st :: ST h) Unit
animate stateM = handleError do
  -- unpack state
  state@{ucRef, usRef, ssRef, scRef, ecRef, esRef, pRef} <- stateM
  uiConf     <- lift $ readSTRef ucRef
  uiST       <- lift $ readSTRef usRef
  systemST   <- lift $ readSTRef ssRef
  systemConf <- lift $ readSTRef scRef
  engineConf <- lift $ readSTRef ecRef
  engineST   <- lift $ readSTRef esRef
  pattern    <- lift $ readSTRef pRef

  -- update time
  currentTimeMS <- lift $ now
  let lastTimeMS = fromMaybe currentTimeMS systemST.lastTimeMS
  let delta = (currentTimeMS - lastTimeMS) * pattern.tSpd / 1000.0
  let t' = systemST.t + delta
  lift $ modifySTRef ssRef (\s -> s {t = t', lastTimeMS = Just currentTimeMS})

  -- fps
  when (systemST.frameNum `mod` uiConf.uiUpdateFreq == 0) do
    let lastFpsTimeMS = fromMaybe currentTimeMS systemST.lastFpsTimeMS
    let fps = round $ (toNumber uiConf.uiUpdateFreq) * 1000.0 / (currentTimeMS - lastFpsTimeMS)
    lift $ modifySTRef ssRef (\s -> s {lastFpsTimeMS = Just currentTimeMS, fps = Just fps})
    return unit

  -- update pattern
  recompile <- runScripts ssRef pRef
  systemST' <- lift $ readSTRef ssRef

  case recompile of -- when doesnt work here for some godforsaken reason
    true -> do
      setShaders systemConf esRef systemST' pattern
    false -> return unit
  engineST' <- lift $ readSTRef esRef

  -- render!
  renderFrame systemST' engineConf engineST' pattern systemST'.frameNum

  -- update ui
  updateLayout uiConf uiST systemST' pattern

  -- request next frame
  lift $ modifySTRef ssRef (\s -> s {frameNum = s.frameNum + 1})
  lift $ requestAnimationFrame $ animate $ return state


main :: Eff (canvas :: Canvas, dom :: DOM, now :: Now) Unit
main = runST do
  animate init
