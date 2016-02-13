module System where

import Prelude
import Data.Maybe (Maybe(Nothing))
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (ExceptT (), lift)
import Control.Monad.ST (ST, STRef, newSTRef)

import Config

defaultSystemST :: SystemST
defaultSystemST = {
    lastTimeMS: Nothing
  , frameNum: 0
  , lastFpsTimeMS: Nothing
  , fps: Nothing
}

-- PUBLIC
initSystemST :: forall eff h. Epi (st :: ST h | eff) (STRef h SystemST)
initSystemST = do
  let systemST = defaultSystemST
  lift $ newSTRef systemST
