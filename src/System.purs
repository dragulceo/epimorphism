module System where

import Prelude
import Data.Maybe

type SystemConf = {
    lastTimeMS :: Maybe Number
  , frameNum :: Int
}

defaultSystemConf :: SystemConf
defaultSystemConf = {
    lastTimeMS: Nothing
  , frameNum: 0
}
