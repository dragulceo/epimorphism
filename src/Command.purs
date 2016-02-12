module Command where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.ST

import Config
import JSUtil (unsafeLog)

command :: forall h eff. (STRef h UIConf) -> (STRef h SystemConf) -> (STRef h EngineConf) -> (STRef h EngineState) -> (STRef h Pattern) -> String -> Eff (st :: ST h | eff) Unit
command ucRef ecRef esRef pRef msg = do
  uiConf      <- readSTRef ucRef
  engineConf  <- readSTRef ecRef
  engineState <- readSTRef esRef
  pattern     <- readSTRef pRef

  unsafeLog (show msg)

  case msg of
    "pause" -> modifySTRef pRef (\p -> p {tSpd = 1.0 - p.tSpd})

  return unit
