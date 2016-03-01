module System where

import Prelude
import Data.Array (foldM) as A
import Data.Either (Either(..))
import Data.List (fromList)
import Data.Maybe (Maybe(..))
import Data.StrMap (empty, lookup, insert, foldM, values, delete, StrMap())
import Data.Tuple (Tuple(..), fst)
import Data.Traversable (traverse)
import Control.Monad.ST (ST, STRef, modifySTRef, newSTRef, readSTRef)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)

import Config
import Util (urlGet, lg, uuid)
import Library
import SLibrary

data DataSource = LocalHTTP | LocalStorage | RemoteDB

initSystemST :: forall eff h. String -> Epi eff (SystemST h)
initSystemST host = do
  -- gather system data here

  systemConfLib <- buildLib buildSystemConf $ host ++ "/lib/system_conf.lib"
  engineConfLib <- buildLib buildEngineConf $ host ++ "/lib/engine_conf.lib"
  uiConfLib     <- buildLib buildUIConf $ host ++ "/lib/ui_conf.lib"
  moduleLib     <- buildLib buildModule $ host ++ "/lib/modules.lib"
  scriptLib     <- buildLib buildScript $ host ++ "/lib/scripts.lib"
  patternLib    <- buildLib buildPattern $ host ++ "/lib/patterns.lib"

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
