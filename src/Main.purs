module Main where

import Prelude
import Data.Maybe (fromMaybe, Maybe(Just))
import Data.Int (round, toNumber)

import Control.Monad (when)
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (ST, STRef, readSTRef, newSTRef, modifySTRef, runST)
import Graphics.Canvas (Canvas)
import DOM (DOM)

import Config (EpiS, Pattern, EngineST, EngineConf, SystemST, SystemConf, UIConf)
import Engine (initEngineST, render, setShaders)
import UI (initUIST, showFps)
import Script (runScripts)
import System (initSystemST, loadLib)
import Util (requestAnimationFrame, now, Now, handleError)
import Pattern (importPattern)

host :: String
host = "http://localhost:8000"

type State h = {
    ucRef :: STRef h UIConf
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
  scRef <- lift $ newSTRef systemConf
  ecRef <- lift $ newSTRef engineConf
  ucRef <- lift $ newSTRef uiConf
  pRef  <- lift $ newSTRef pattern

  -- import pattern
  importPattern ssRef pRef
  pattern'  <- lift $ readSTRef pRef

  lift $ modifySTRef ssRef (\s -> s {mainRef = pattern'.main})
  systemST' <- lift $ readSTRef ssRef

  -- init engine & ui states
  esRef <- initEngineST systemConf engineConf systemST' pattern' uiConf.canvasId
  initUIST ucRef ecRef esRef pRef scRef ssRef

  return {ucRef, ssRef, scRef, ecRef, esRef, pRef}


animate :: forall h. EpiS (now :: Now) h (State h) -> Eff (canvas :: Canvas, dom :: DOM, now :: Now, st :: ST h) Unit
animate stateM = handleError do
  -- unpack state
  state@{ucRef, ssRef, scRef, ecRef, esRef, pRef} <- stateM
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
  let freq = 10
  when (systemST.frameNum `mod` freq == 0) do
    let lastFpsTimeMS = fromMaybe currentTimeMS systemST.lastFpsTimeMS
    let fps = round $ (toNumber freq) * 1000.0 / (currentTimeMS - lastFpsTimeMS)
    lift $ modifySTRef ssRef (\s -> s {lastFpsTimeMS = Just currentTimeMS, fps = Just fps})
    showFps fps

  -- update pattern

  recompile <- runScripts ssRef
  systemST' <- lift $ readSTRef ssRef

  case recompile of -- when doesnt work here for some godforsaken reason
    true -> do
      setShaders systemConf esRef systemST' pattern
    false -> return unit
  engineST'   <- lift $ readSTRef esRef

  -- render
  render systemST' engineConf engineST' pattern systemST'.frameNum

  -- request next frame
  lift $ modifySTRef ssRef (\s -> s {frameNum = s.frameNum + 1})
  lift $ requestAnimationFrame $ animate $ return state


main :: Eff (canvas :: Canvas, dom :: DOM, now :: Now) Unit
main = runST do
  animate init
