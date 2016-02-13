module System where

import Prelude (return)
import Data.Maybe (Maybe(Nothing))
import Data.StrMap (StrMap (), empty)
import Control.Monad.ST (ST)

import Config

data DataSource = LocalHTTP | LocalStorage | RemoteDB

defaultSystemConf :: SystemConf
defaultSystemConf = {
    initEngineConf: "default"
  , initUIConf: "default"
  , initPattern: "default"
}

defaultSystemST :: SystemST
defaultSystemST = {
    lastTimeMS: Nothing
  , frameNum: 0
  , lastFpsTimeMS: Nothing
  , fps: Nothing
  , uiConfigLib: empty
  , engineConfigLib: empty
  , patternLib: empty
  , moduleLib: empty
  , shaderLib: empty
  , componentLib: empty
  , libraryLib: empty
}


initSystemST :: forall eff h. SystemConf -> Epi (st :: ST h | eff) SystemST
initSystemST systemConf = do
  let st = defaultSystemST
  -- load libraries
  return st
