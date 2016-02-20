module System where

import Prelude
import Data.Array (foldM) as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (empty, lookup, insert, foldM, keys, StrMap())
import Data.Tuple (Tuple(..), fst)
import Control.Monad.ST (ST, STRef, modifySTRef, newSTRef, readSTRef)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)

import Config
import JSUtil (unsafeURLGet, unsafeLog, reallyUnsafeLog)
import Library
import SLibrary

data DataSource = LocalHTTP | LocalStorage | RemoteDB

defaultSystemST :: forall h. SystemST h
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
  , moduleRefLib: empty
  , componentLib: empty
  , indexLib: empty
}


initSystemST :: forall eff h . Epi eff (SystemST h)
initSystemST = do
  systemConfLib <- buildLib buildSystemConf "lib/system_conf.lib"
  engineConfLib <- buildLib buildEngineConf "lib/engine_conf.lib"
  uiConfLib     <- buildLib buildUIConf "lib/ui_conf.lib"
  moduleLib     <- buildLib buildModule "lib/modules.lib"
  patternLib    <- buildLib buildPattern "lib/patterns.lib"
  componentLib  <- buildSLib buildComponent "lib/components.slib"
  indexLib      <- buildSLib buildIndex "lib/indexes.slib"
  return $ defaultSystemST {
      systemConfLib = systemConfLib
    , engineConfLib = engineConfLib
    , uiConfLib     = uiConfLib
    , moduleLib     = moduleLib
    , patternLib    = patternLib
    , componentLib  = componentLib
    , indexLib      = indexLib
  }


buildModuleRefLib :: forall h eff. (STRef h (SystemST h)) -> Pattern -> Epi (st :: ST h | eff) Unit
buildModuleRefLib ssRef pattern = do
  systemST <- lift $ readSTRef ssRef

  mrl   <- handle (Tuple empty systemST) pattern.main
  mrl'  <- handle mrl  pattern.disp
  mrl'' <- handle mrl' pattern.vert

  lift $ modifySTRef ssRef (\s -> s { moduleRefLib = (fst mrl'') })
  return unit
  where
    handle :: forall h eff. (Tuple (StrMap (STRef h Module)) (SystemST h)) -> String -> Epi (st :: ST h | eff) (Tuple (StrMap (STRef h Module)) (SystemST h))
    handle (Tuple dt systemST) n = do
      return $ Tuple dt systemST
      m <- loadLib n systemST.moduleLib
      ref <- lift $ newSTRef m
      let dt' = insert n ref dt
      A.foldM handle (Tuple dt' systemST) (keys m.modules)


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
    Nothing  -> throwError ("Load from lib - can't find: " ++ name)


-- should this go here?
loadModules :: forall eff. StrMap ModRef -> (StrMap Module) -> Epi eff (StrMap Module)
loadModules mr lib = do
  foldM handle empty mr
  where
    handle dt k v = do
      m <- loadLib v lib
      return $ insert k m dt
