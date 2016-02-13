module Main where

import Prelude (Unit, unit, return, bind, ($), (*), (-), (+), (/), (==), id, mod)
import Data.Either (Either(Right, Left))
import Data.Maybe (maybe, Maybe(Just))
import Data.Int (round)

import Control.Monad (when)
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (runExceptT, lift)
import Control.Monad.ST (ST, STRef, writeSTRef, readSTRef, newSTRef, modifySTRef, runST)
import Graphics.Canvas (Canvas)
import DOM (DOM)

import Config (Epi, Pattern, EngineState, EngineConf, SystemState)
import Engine (loadEngineConf, initEngine, render)
import UI (loadUIConf, initUIState, showFps)
import Pattern (loadPattern, updatePattern)
import System (initSystemState)
import JSUtil (unsafeLog, requestAnimationFrame, now, Now)

main :: Eff (canvas :: Canvas, dom :: DOM, now :: Now) Unit
main = runST do
  res <- runExceptT doMain
  case res of
    Left er -> unsafeLog er
    Right _ -> return unit

-- this is in its own method for type inference reasons
doMain :: forall h. Epi (now :: Now, st :: ST h) Unit
doMain = do
  -- init config
  engineConf <- loadEngineConf "default"
  uiConf     <- loadUIConf     "default"
  pattern    <- loadPattern    "default"

  ecRef <- lift $ newSTRef engineConf
  ucRef <- lift $ newSTRef uiConf
  pRef  <- lift $ newSTRef pattern

  -- init states
  esRef <- initEngine uiConf.canvasId ecRef pRef
  ssRef <- initSystemState
  initUIState ucRef ecRef esRef pRef

  -- go!
  animate ssRef ecRef esRef pRef


animate :: forall h. (STRef h SystemState) -> (STRef h EngineConf) -> (STRef h EngineState) -> (STRef h Pattern) -> Epi (now :: Now, st :: ST h) Unit
animate ssRef ecRef esRef pRef = do
  systemState <- lift $ readSTRef ssRef
  engineConf  <- lift $ readSTRef ecRef
  engineState <- lift $ readSTRef esRef
  pattern     <- lift $ readSTRef pRef

  -- update time
  currentTimeMS <- lift $ now
  let lastTimeMS = maybe currentTimeMS id systemState.lastTimeMS
  let delta = (currentTimeMS - lastTimeMS) * pattern.tSpd
  lift $ modifySTRef ssRef (\s -> s {lastTimeMS = Just currentTimeMS})

  -- fps
  when (systemState.frameNum `mod` 10 == 0) do
    let lastFpsTimeMS = maybe currentTimeMS id systemState.lastFpsTimeMS
    let fps = round $ 10.0 * 1000.0 / (currentTimeMS - lastFpsTimeMS)
    lift $ modifySTRef ssRef (\s -> s {lastFpsTimeMS = Just currentTimeMS, fps = Just fps})
    showFps fps

  -- update pattern & render
  let pattern' = updatePattern pattern (pattern.t + delta)
  lift $ writeSTRef pRef pattern'
  render engineConf engineState pattern' systemState.frameNum

  -- request next frame
  lift $ modifySTRef ssRef (\s -> s {frameNum = s.frameNum + 1})
  lift $ requestAnimationFrame $ runExceptT (animate ssRef ecRef esRef pRef)
