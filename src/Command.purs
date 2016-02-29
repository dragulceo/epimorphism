module Command where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.ST

import Config
import Util (lg)

command :: forall h eff. STRef h UIConf -> STRef h EngineConf -> STRef h EngineST -> STRef h Pattern -> String -> Eff (st :: ST h | eff) Unit
command ucRef ecRef esRef pRef msg = do
  uiConf     <- readSTRef ucRef
  engineConf <- readSTRef ecRef
  engineST   <- readSTRef esRef
  pattern    <- readSTRef pRef

  let x = lg (show msg)

  case msg of
    "pause" -> modifySTRef pRef (\p -> p {tSpd = 1.0 - p.tSpd})

  return unit
