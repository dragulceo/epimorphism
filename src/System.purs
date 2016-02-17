module System where

import Prelude
import Data.Maybe (Maybe(..))
import Data.StrMap (empty, lookup, StrMap())
import Data.Either (Either(..))
import Control.Monad.ST (ST)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)

import Config
import JSUtil (unsafeURLGet, unsafeLog)
import Library

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
  engineConfLib <- buildLib buildEngineConf "lib/engine_conf.lib"
  uiConfLib     <- buildLib buildUIConf "lib/ui_conf.lib"
  return $ st {engineConfLib = engineConfLib,
               uiConfLib = uiConfLib}
  where
    buildLib :: forall a.  (StrMap LineVal -> Lib a) -> String -> Epi (st :: ST h | eff) (StrMap a)
    buildLib f loc = do
      dta <- lift $ unsafeURLGet loc
      case (parseLib f dta) of
        (Right res) -> return res
        (Left (LibError s)) -> throwError s

loadConf :: forall eff h a. String -> (StrMap a) -> Epi (st :: ST h | eff) a
loadConf name lib = do
  case (lookup name lib) of
    (Just d) -> return d
    Nothing  -> throwError ("can't find library: " ++ name)
