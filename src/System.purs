module System where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (empty, lookup, StrMap())
import Data.Tuple (Tuple(..))
import Control.Monad.ST (ST)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)

import Config
import JSUtil (unsafeURLGet, unsafeLog, reallyUnsafeLog)
import Library
import SLibrary

data DataSource = LocalHTTP | LocalStorage | RemoteDB

defaultSystemST :: SystemST
defaultSystemST = {
    lastTimeMS: Nothing
  , frameNum: 0
  , lastFpsTimeMS: Nothing
  , fps: Nothing
  , systemConfLib: empty
  , uiConfLib: empty
  , engineConfLib: empty
  , patternLib: empty
  , moduleLib: empty
  , shaderLib: empty
  , componentLib: empty
  , indexLib: empty
}


initSystemST :: forall eff. Epi eff SystemST
initSystemST = do
  systemConfLib <- buildLib buildSystemConf "lib/system_conf.lib"
  engineConfLib <- buildLib buildEngineConf "lib/engine_conf.lib"
  uiConfLib     <- buildLib buildUIConf "lib/ui_conf.lib"
  moduleLib     <- buildLib buildModule "lib/modules.lib"
  patternLib    <- buildLib buildPattern "lib/patterns.lib"
  componentLib  <- buildSLib buildComponent "lib/components.slib"
  indexLib      <- buildSLib buildIndex "lib/indexes.slib"
  shaderLib     <- buildSLib buildShader "lib/shaders.slib"
  let x = reallyUnsafeLog shaderLib
  return $ defaultSystemST {
      systemConfLib = systemConfLib
    , engineConfLib = engineConfLib
    , uiConfLib     = uiConfLib
    , moduleLib     = moduleLib
    , patternLib    = patternLib
    , componentLib  = componentLib
    , indexLib      = indexLib
    , shaderLib     = shaderLib
  }


buildLib :: forall a eff.  (StrMap LineVal -> Lib a) -> String -> Epi eff (StrMap a)
buildLib f loc = do
  dta <- lift $ unsafeURLGet loc
  case (parseLib f dta) of
    (Right res) -> return res
    (Left (LibError s)) -> throwError s


buildSLib :: forall a eff.  (SHandle -> SLib (Tuple String a)) -> String -> Epi eff (StrMap a)
buildSLib f loc = do
  dta <- lift $ unsafeURLGet loc
  case (parseSLib f dta) of
    (Right res) -> return res
    (Left (SLibError s)) -> throwError s


loadLib :: forall a eff. String -> (StrMap a) -> Epi eff a
loadLib name lib = do
  case (lookup name lib) of
    (Just d) -> return d
    Nothing  -> throwError ("can't find library: " ++ name)
