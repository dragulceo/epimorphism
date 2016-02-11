module Main where

import Prelude (Unit, unit, return, bind, ($), (*), (-), (+), show)
import Data.Either (Either(Right, Left))
import Data.Maybe (maybe, Maybe(Just))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Alert (Alert)
import Control.Monad.Except.Trans (runExceptT, lift, ExceptT ())
import Control.Monad.ST (ST, STRef, writeSTRef, readSTRef, newSTRef, modifySTRef, runST)
import Graphics.Canvas (Canvas)

import Engine (loadEngineConf, initEngine, render, EngineState, EngineConf)
import UI (loadUIConf)
import Pattern (loadPattern, updatePattern, Pattern)
import JSUtil (unsafeLog, requestAnimationFrame, now, Now)
import System (SystemConf, defaultSystemConf)

type Epi eff = Eff (console :: CONSOLE, alert :: Alert, canvas :: Canvas, now :: Now | eff)

main :: Epi () Unit
main = runST do
  res <- runExceptT doMain
  case res of
    Left er -> log $ show er
    Right _ -> return unit

doMain :: forall h. ExceptT String (Epi (st :: ST h)) Unit
doMain = do
    let systemConf = defaultSystemConf
    engineConf <- loadEngineConf "default"
    uiConf     <- loadUIConf     "default"
    pattern    <- loadPattern    "default"

    scRef  <- lift $ newSTRef systemConf
    ecRef  <- lift $ newSTRef engineConf
    uicRef <- lift $ newSTRef uiConf
    pRef   <- lift $ newSTRef pattern

    esRef <- initEngine ecRef pRef

    -- register ui handlers
    animate scRef ecRef esRef pRef

animate :: forall h. (STRef h SystemConf) -> (STRef h EngineConf) -> (STRef h EngineState) -> (STRef h Pattern) -> ExceptT String (Epi (st :: ST h)) Unit
animate scRef ecRef esRef pRef = do
  systemConf  <- lift $ readSTRef scRef
  engineConf  <- lift $ readSTRef ecRef
  engineState <- lift $ readSTRef esRef
  pattern     <- lift $ readSTRef pRef

  -- update time
  currentTimeMS <- lift $ now
  let lastTimeMS = maybe currentTimeMS (\a -> a) systemConf.lastTimeMS
  let delta = (currentTimeMS - lastTimeMS) * pattern.tSpd
  lift $ modifySTRef scRef (\s -> s {lastTimeMS = Just currentTimeMS})
  -- lift $ unsafeLog pRef

  let pattern' = updatePattern pattern (pattern.t + delta)
  lift $ writeSTRef pRef pattern'
  render engineConf engineState pattern' systemConf.frameNum

  lift $ modifySTRef scRef (\s -> s {frameNum = s.frameNum + 1})
  lift $ requestAnimationFrame $ runExceptT (animate scRef ecRef esRef pRef)
