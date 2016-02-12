module System where

import Prelude
import Data.Maybe (Maybe(Nothing))

import Config

defaultSystemConf :: SystemConf
defaultSystemConf = {
    lastTimeMS: Nothing
  , frameNum: 0
}
