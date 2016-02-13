module System where

import Prelude (return)
import Data.Maybe (Maybe(Nothing))
import Control.Monad.ST (ST)

import Config

defaultSystemST :: SystemST
defaultSystemST = {
    lastTimeMS: Nothing
  , frameNum: 0
  , lastFpsTimeMS: Nothing
  , fps: Nothing
}

-- PUBLIC
initSystemST :: forall eff h. Epi (st :: ST h | eff) SystemST
initSystemST = do
  return defaultSystemST
