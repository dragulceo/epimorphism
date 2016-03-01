module Command where

import Prelude

import Data.Array (length, head, tail)
import Data.Maybe.Unsafe (fromJust)
import Data.String (split)

import Control.Monad (unless)
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (runExceptT, lift)
import Control.Monad.ST

import Graphics.Canvas (Canvas)
import DOM (DOM)


import Config
import Util (winLog, lg, handleError)

command :: forall eff h. STRef h UIConf -> STRef h EngineConf -> STRef h EngineST -> STRef h Pattern -> STRef h SystemConf -> STRef h (SystemST h) -> String -> Eff (canvas :: Canvas, dom :: DOM, st :: ST h | eff) Unit
command ucRef ecRef esRef pRef scRef ssRef msg = handleError do
  uiConf     <- lift $ readSTRef ucRef
  engineConf <- lift $ readSTRef ecRef
  engineST   <- lift $ readSTRef esRef
  pattern    <- lift $ readSTRef pRef

  let x = lg (show msg)

  let dt = split " " msg

  unless (length dt == 0) do
    let cmd = fromJust $ head dt
    let args = fromJust $ tail dt

    case cmd of
      "null" -> return unit
      "pause" -> do
        lift $ modifySTRef pRef (\p -> p {tSpd = 1.0 - p.tSpd})
        return unit
      "scr" -> do
        return unit
      _ -> throwError $ "Unknown command: " ++ msg
