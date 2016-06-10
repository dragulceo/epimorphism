module System where

import SLibrary
import Config (scriptSchema, moduleSchema, patternSchema, systemConfSchema, uiConfSchema, engineConfSchema, Schema, Epi, SystemST, defaultSystemST)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (lookup, StrMap)
import Data.Tuple (Tuple)
import Library (LibError(..), parseLib)
import Prelude ((++), return, ($), bind)
import Util (urlGet)

data DataSource = LocalHTTP | LocalStorage | RemoteDB

initSystemST :: forall eff h. String -> Epi eff (SystemST h)
initSystemST host = do
  -- gather system data here

  -- initialize libraries
  systemConfLib <- buildLib systemConfSchema $ host ++ "/lib/system_conf.lib"
  engineConfLib <- buildLib engineConfSchema $ host ++ "/lib/engine_conf.lib"
  uiConfLib     <- buildLib uiConfSchema     $ host ++ "/lib/ui_conf.lib"
  moduleLib     <- buildLib moduleSchema     $ host ++ "/lib/modules.lib"
  scriptLib     <- buildLib scriptSchema     $ host ++ "/lib/scripts.lib"
  patternLib    <- buildLib patternSchema    $ host ++ "/lib/patterns.lib"

  componentLib  <- buildSLib buildComponent  $ host ++ "/lib/components.slib"
  indexLib      <- buildSLib buildIndex      $ host ++ "/lib/indexes.slib"

  return $ defaultSystemST {
      systemConfLib = systemConfLib
    , engineConfLib = engineConfLib
    , uiConfLib     = uiConfLib
    , moduleLib     = moduleLib
    , scriptLib     = scriptLib
    , patternLib    = patternLib
    , componentLib  = componentLib
    , indexLib      = indexLib
  }

buildLib :: forall eff a. Schema -> String -> Epi eff (StrMap a)
buildLib schema loc = do
  dt <- lift $ urlGet loc
  case dt of
    (Left er) -> throwError $ "Error loading lib : " ++ er
    (Right res) -> case (parseLib schema res) of
      (Right res') -> return res'
      (Left (LibError s)) -> throwError $ "Error building lib at : " ++ loc ++ " : " ++ s

-- build a shader library from a location with a builder
buildSLib :: forall eff a.  (SHandle -> SLib (Tuple String a)) -> String -> Epi eff (StrMap a)
buildSLib f loc = do
  dt <- lift $ urlGet loc
  case dt of
    (Left er) -> throwError $ "Error loading slib : " ++ er
    (Right res) -> case (parseSLib f res) of
      (Right res') -> return res'
      (Left (SLibError s)) -> throwError $ "Error building slib at : " ++ loc ++ " : " ++ s


-- load from a map, throw error if not found. passed context for debugging purposes
loadLib :: forall eff a. String -> (StrMap a) -> String -> Epi eff a
loadLib name lib ctx = do
  case (lookup name lib) of
    (Just d) -> return d
    Nothing  -> throwError ("Load from lib - can't find: " ++ name ++ ": context :" ++ ctx)
