module Main where

import Prelude
import Data.Either (Either(..), either)
import Data.Maybe (fromMaybe, Maybe(Just))
import Data.Int (round, toNumber)

import Control.Monad (when)
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (runExceptT, lift)
import Control.Monad.ST (ST, STRef, writeSTRef, readSTRef, newSTRef, modifySTRef, runST)
import Graphics.Canvas (Canvas)
import DOM (DOM)

import Config
import Engine (initEngineST, render)
import UI (initUIST, showFps)
import Script (runScripts)
import System (initSystemST, loadLib, importPattern)
import Util (winLog, lg, requestAnimationFrame, now, Now)

host :: String
host = "http://localhost:8000"

type State h = {
    ucRef :: STRef h UIConf
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
  systemST' <- lift $ readSTRef ssRef
  let x = lg systemST'

  -- init states
  esRef <- initEngineST systemConf engineConf systemST' pattern uiConf.canvasId
  initUIST ucRef ecRef esRef pRef scRef ssRef

  return {ucRef, ssRef, ecRef, esRef, pRef}


animate :: forall h. EpiS (now :: Now) h (State h) -> Eff (canvas :: Canvas, dom :: DOM, now :: Now, st :: ST h) Unit
animate stateM = handleError do
  -- unpack state
  state@{ucRef, ssRef, ecRef, esRef, pRef} <- stateM
  systemST   <- lift $ readSTRef ssRef
  engineConf <- lift $ readSTRef ecRef
  engineST   <- lift $ readSTRef esRef
  pattern    <- lift $ readSTRef pRef

  -- update time
  currentTimeMS <- lift $ now
  let lastTimeMS = fromMaybe currentTimeMS systemST.lastTimeMS
  let delta = (currentTimeMS - lastTimeMS) * pattern.tSpd
  lift $ modifySTRef ssRef (\s -> s {lastTimeMS = Just currentTimeMS})

  -- fps
  let freq = 10
  when (systemST.frameNum `mod` freq == 0) do
    let lastFpsTimeMS = fromMaybe currentTimeMS systemST.lastFpsTimeMS
    let fps = round $ (toNumber freq) * 1000.0 / (currentTimeMS - lastFpsTimeMS)
    lift $ modifySTRef ssRef (\s -> s {lastFpsTimeMS = Just currentTimeMS, fps = Just fps})
    showFps fps

  -- update pattern
  let t' = pattern.t + delta
  lift $ modifySTRef pRef (\p -> p {t = t'})
  pattern' <- lift $ readSTRef pRef
  runScripts t' systemST.scriptRefPool systemST.moduleRefPool

  -- render
  render systemST engineConf engineST pattern' systemST.frameNum

  -- request next frame
  lift $ modifySTRef ssRef (\s -> s {frameNum = s.frameNum + 1})
  lift $ requestAnimationFrame $ animate $ return state


handleError :: forall eff. Epi eff Unit -> Eff (canvas :: Canvas, dom :: DOM | eff) Unit
handleError epi = do
  res <- runExceptT epi
  either winLog return res


main :: Eff (canvas :: Canvas, dom :: DOM, now :: Now) Unit
main = runST do
  animate init
