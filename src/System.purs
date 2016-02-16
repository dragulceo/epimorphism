module System where

import Prelude
import Data.Maybe (Maybe(..))
import Data.StrMap (empty)
import Control.Monad.ST (ST)

import Config
import JSUtil (unsafeURLGet, unsafeLog)

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
  , uiConfLib: empty
  , engineConfLib: empty
  , patternLib: empty
  , moduleLib: empty
  , shaderLib: empty
  , componentLib: empty
  , libraryLib: empty
}


initSystemST :: forall eff h. SystemConf -> Epi (st :: ST h | eff) SystemST
initSystemST systemConf = do
  let st = defaultSystemST
  -- engine

 -- let st' = st {engineConfLib = engineConfLib}
  return st
