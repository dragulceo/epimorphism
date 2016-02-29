module Main where

import Prelude
import Data.Either (Either(..), either)
import Data.Maybe (maybe, Maybe(Just))
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
import System (initSystemST, loadLib, buildRefPools)
import JSUtil (winLog, reallyUnsafeLog, requestAnimationFrame, now, Now)

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
  systemST <- initSystemST
  ssRef <- lift $ newSTRef systemST

  -- init config
  systemConf <- loadLib "default" systemST.systemConfLib
  engineConf <- loadLib systemConf.initEngineConf systemST.engineConfLib
  uiConf     <- loadLib systemConf.initUIConf systemST.uiConfLib
  pattern    <- loadLib systemConf.initPattern systemST.patternLib

  -- build reference libs
  buildRefPools ssRef pattern
  systemST' <- lift $ readSTRef ssRef

  ecRef <- lift $ newSTRef engineConf
  ucRef <- lift $ newSTRef uiConf
  pRef  <- lift $ newSTRef pattern

  -- init states
  esRef <- initEngineST engineConf systemST' pattern uiConf.canvasId
  initUIST ucRef ecRef esRef pRef

  return { ucRef: ucRef, ssRef: ssRef, ecRef: ecRef, esRef: esRef, pRef: pRef }


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
  let lastTimeMS = maybe currentTimeMS id systemST.lastTimeMS
  let delta = (currentTimeMS - lastTimeMS) * pattern.tSpd
  lift $ modifySTRef ssRef (\s -> s {lastTimeMS = Just currentTimeMS})

  -- fps
  let freq = 10
  when (systemST.frameNum `mod` freq == 0) do
    let lastFpsTimeMS = maybe currentTimeMS id systemST.lastFpsTimeMS
    let fps = round $ (toNumber freq) * 1000.0 / (currentTimeMS - lastFpsTimeMS)
    lift $ modifySTRef ssRef (\s -> s {lastFpsTimeMS = Just currentTimeMS, fps = Just fps})
    showFps fps

  -- update pattern & render
  let t' = pattern.t + delta
  lift $ modifySTRef pRef (\p -> p {t = t'})
  pattern' <- lift $ readSTRef pRef
  runScripts t' systemST.scriptRefPool systemST.moduleRefPool
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
