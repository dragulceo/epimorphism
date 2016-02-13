module System where

import Prelude
import Data.Maybe (Maybe(Nothing))
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (ExceptT (), lift)
import Control.Monad.ST (ST, STRef, newSTRef)

import Config

defaultSystemState :: SystemState
defaultSystemState = {
    lastTimeMS: Nothing
  , frameNum: 0
  , lastFpsTimeMS: Nothing
  , fps: Nothing
}

-- PUBLIC
initSystemState :: forall eff h. ExceptT String (Eff (st :: ST h | eff)) (STRef h SystemState)
initSystemState = do
  let systemState = defaultSystemState
  lift $ newSTRef systemState
