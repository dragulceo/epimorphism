module System where

import SLibrary
import Config (testObjSchema, Schema, Epi, SystemST, defaultSystemST)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (lookup, StrMap)
import Data.Tuple (Tuple)
import Library (Lib, LineVal, LibError(LibError), parseLib, buildPattern, buildScript, buildModule, buildUIConf, buildEngineConf, buildSystemConf)
import Library2 (LibError2(..), parseLib2)
import Prelude ((++), return, ($), bind)
import Util (urlGet, lg)

data DataSource = LocalHTTP | LocalStorage | RemoteDB

initSystemST :: forall eff h. String -> Epi eff (SystemST h)
initSystemST host = do
  -- gather system data here

  -- initialize libraries
  systemConfLib <- buildLib buildSystemConf $ host ++ "/lib/system_conf.lib"
  engineConfLib <- buildLib buildEngineConf $ host ++ "/lib/engine_conf.lib"
  uiConfLib     <- buildLib buildUIConf $ host ++ "/lib/ui_conf.lib"
  moduleLib     <- buildLib buildModule $ host ++ "/lib/modules.lib"
  scriptLib     <- buildLib buildScript $ host ++ "/lib/scripts.lib"
  patternLib    <- buildLib buildPattern $ host ++ "/lib/patterns.lib"
  testObjLib    <- buildLib2 testObjSchema $ host ++ "/lib/test_obj.lib"
  let b = lg testObjLib

  componentLib  <- buildSLib buildComponent $ host ++ "/lib/components.slib"
  indexLib      <- buildSLib buildIndex $ host ++ "/lib/indexes.slib"

  return $ defaultSystemST {
      systemConfLib = systemConfLib
    , engineConfLib = engineConfLib
    , uiConfLib     = uiConfLib
    , moduleLib     = moduleLib
    , scriptLib     = scriptLib
    , patternLib    = patternLib
    , componentLib  = componentLib
    , indexLib      = indexLib
    , testObjLib    = testObjLib
  }


-- build a library from a location with a builder
buildLib :: forall eff a.  (StrMap LineVal -> Lib a) -> String -> Epi eff (StrMap a)
buildLib f loc = do
  dt <- lift $ urlGet loc
  case dt of
    (Left er) -> throwError $ "Error loading lib : " ++ er
    (Right res) -> case (parseLib f res) of
      (Right res') -> return res'
      (Left (LibError s)) -> throwError $ "Error building lib at : " ++ loc ++ " : " ++ s


buildLib2 :: forall eff a. Schema -> String -> Epi eff (StrMap a)
buildLib2 schema loc = do
  dt <- lift $ urlGet loc
  case dt of
    (Left er) -> throwError $ "Error loading lib : " ++ er
    (Right res) -> case (parseLib2 schema res) of
      (Right res') -> return res'
      (Left (LibError2 s)) -> throwError $ "Error building lib at : " ++ loc ++ " : " ++ s

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
