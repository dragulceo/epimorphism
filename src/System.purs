module System where

import Prelude
import Data.Array (foldM) as A
import Data.Either (Either(..))
import Data.List (fromList)
import Data.Maybe (Maybe(..))
import Data.StrMap (empty, lookup, insert, foldM, values, StrMap())
import Data.Tuple (Tuple(..), fst)
import Control.Monad.ST (ST, STRef, modifySTRef, newSTRef, readSTRef)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)

import Config
import Util (urlGet, lg)
import Library
import SLibrary

data DataSource = LocalHTTP | LocalStorage | RemoteDB

initSystemST :: forall eff h . Epi eff (SystemST h)
initSystemST = do
  systemConfLib <- buildLib buildSystemConf "lib/system_conf.lib"
  engineConfLib <- buildLib buildEngineConf "lib/engine_conf.lib"
  uiConfLib     <- buildLib buildUIConf "lib/ui_conf.lib"
  moduleLib     <- buildLib buildModule "lib/modules.lib"
  scriptLib     <- buildLib buildScript "lib/scripts.lib"
  patternLib    <- buildLib buildPattern "lib/patterns.lib"

  componentLib  <- buildSLib buildComponent "lib/components.slib"
  indexLib      <- buildSLib buildIndex "lib/indexes.slib"

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


-- this method flattens the loaded module structure into a maps of StRefs for both modules &
-- patterns.  the idea is that we only carry around names of these things anywhere
-- and when we want one, we look it up in these pools.
type RData h = {mdt :: StrMap (STRef h Module), sdt :: StrMap (STRef h Script)}
buildRefPools :: forall h eff. STRef h (SystemST h) -> Pattern -> EpiS eff h Unit
buildRefPools ssRef pattern = do
  systemST <- lift $ readSTRef ssRef

  -- recursively load all modules
  let dt = {mdt: empty, sdt: empty}
  dt' <- A.foldM (handle systemST) dt [pattern.main, pattern.disp, pattern.vert]

  lift $ modifySTRef ssRef (\s -> s {moduleRefPool = dt'.mdt, scriptRefPool = dt'.sdt})
  return unit
  where
    handle :: forall h eff. SystemST h -> (RData h) -> String -> Epi (st :: ST h | eff) (RData h)
    handle st dt@{mdt, sdt} n = do
      m <- loadLib n st.moduleLib
      ref <- lift $ newSTRef m

      -- scripts
      let mdt' = insert n ref mdt
      sdt' <- A.foldM (handleS st) sdt m.scripts

      -- recurse
      let dt' = dt {mdt = mdt', sdt = sdt'}
      A.foldM (handle st) dt' (fromList $ values m.modules)

    handleS :: forall h eff. SystemST h -> StrMap (STRef h Script) -> String -> EpiS eff h (StrMap (STRef h Script))
    handleS st sdt n = do
      s <- loadLib n st.scriptLib
      ref <- lift $ newSTRef s
      return $ insert n ref sdt


buildLib :: forall a eff.  (StrMap LineVal -> Lib a) -> String -> Epi eff (StrMap a)
buildLib f loc = do
  dt <- lift $ urlGet loc
  case dt of
    (Left er) -> throwError $ "Error loading lib : " ++ er
    (Right res) -> case (parseLib f res) of
      (Right res') -> return res'
      (Left (LibError s)) -> throwError $ "Error building lib at : " ++ loc ++ " : " ++ s


buildSLib :: forall a eff.  (SHandle -> SLib (Tuple String a)) -> String -> Epi eff (StrMap a)
buildSLib f loc = do
  dt <- lift $ urlGet loc
  case dt of
    (Left er) -> throwError $ "Error loading slib : " ++ er
    (Right res) -> case (parseSLib f res) of
      (Right res') -> return res'
      (Left (SLibError s)) -> throwError $ "Error building slib at : " ++ loc ++ " : " ++ s

loadLib :: forall a eff. String -> (StrMap a) -> Epi eff a
loadLib name lib = do
  case (lookup name lib) of
    (Just d) -> return d
    Nothing  -> throwError ("Load from lib - can't find: " ++ name)
