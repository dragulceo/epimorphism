module System where

import Prelude
import Data.Maybe (Maybe(Nothing))
import Control.Monad.Trans (lift)
import Control.Monad.ST (newSTRef)

import Config

defaultSystemState :: SystemState
defaultSystemState = {
    lastTimeMS: Nothing
  , frameNum: 0
  , lastFpsTimeMS: Nothing
  , fps: Nothing
}

-- PUBLIC
initSystemState :: forall eff. String -> ExceptT String (Eff eff) SystemState
initSystemState name = do
  let systemState = defaultSystemState
  lift $ newSTRef systemState
