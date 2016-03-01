module Pattern where

import Prelude
import Data.Maybe (Maybe(..))
import Data.StrMap (empty, lookup, insert, foldM, values, delete, StrMap())
import Data.Traversable (traverse)
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT (), lift)
import Control.Monad.ST (ST, STRef, modifySTRef, newSTRef, readSTRef)


import Config
import System (loadLib)
import Util (uuid, lg)


-- import the modules of a pattern into the ref pool
type RData h = {mdt :: StrMap (STRef h Module), sdt :: StrMap (STRef h Script), name :: String}
importPattern :: forall h eff. STRef h (SystemST h) -> STRef h Pattern -> EpiS eff h Unit
importPattern ssRef pRef =  do
  systemST <- lift $ readSTRef ssRef
  pattern  <- lift $ readSTRef pRef

  -- import all modules
  main <- importModule ssRef pattern.main
  disp <- importModule ssRef pattern.disp
  vert <- importModule ssRef pattern.vert
  lift $ modifySTRef pRef (\p -> p {main = main, disp = disp, vert = vert})

  return unit


-- import a module into the ref pool
importModule :: forall h eff. STRef h (SystemST h) -> String -> EpiS eff h String
importModule ssRef n = do
  systemST <- lift $ readSTRef ssRef
  m <- loadLib n systemST.moduleLib "import module"
  id <- lift $ uuid

  -- import children
  m' <- foldM (importChild ssRef) m m.modules
  systemST' <- lift $ readSTRef ssRef

  -- update scripts
  scripts' <- traverse (importScript ssRef id) m.scripts
  let m'' = m' {scripts = scripts'}

  -- update pool
  ref <- lift $ newSTRef m''
  let mp = insert id ref systemST'.moduleRefPool  -- maybe check for duplicates here?
  lift $ modifySTRef ssRef (\s -> s {moduleRefPool = mp})

  return id
  where
    importChild :: STRef h (SystemST h) -> Module -> String -> String -> EpiS eff h Module
    importChild ssRef m k v = do
      child <- importModule ssRef v
      let modules' = insert k child m.modules
      return $ m {modules = modules'}


-- remove a module from the ref pool
purgeModule :: forall h eff. STRef h (SystemST h) -> String -> EpiS eff h Unit
purgeModule ssRef mid = do
  systemST <- lift $ readSTRef ssRef
  let mM = lookup mid systemST.moduleRefPool
  case mM of
    Nothing -> throwError $ "Can find & purge module : " ++ mid
    Just mRef -> do
      mod <- lift $ readSTRef mRef
      -- purge scripts
      traverse (purgeScript ssRef) mod.scripts

      -- delete self
      let mp = delete mid systemST.moduleRefPool
      lift $ modifySTRef ssRef (\s -> s {moduleRefPool = mp})

      -- purge children

      traverse (purgeModule ssRef) (values mod.modules)
      return unit


-- import a script into the ref pool
importScript :: forall h eff. STRef h (SystemST h) -> String -> String -> EpiS eff h String
importScript ssRef mid sn = do
  systemST <- lift $ readSTRef ssRef
  s <- loadLib sn systemST.scriptLib "import script"
  id <- lift $ uuid

  -- add module
  let s' = s {mod = Just mid}
  ref <- lift $ newSTRef s'

  --update pool
  let sp = insert id ref systemST.scriptRefPool
  lift $ modifySTRef ssRef (\s -> s {scriptRefPool = sp})

  return id


-- remove a script from the ref pool
purgeScript :: forall h eff. STRef h (SystemST h) -> String -> EpiS eff h Unit
purgeScript ssRef sid = do
  systemST <- lift $ readSTRef ssRef
  let sM = lookup sid systemST.scriptRefPool
  case sM of
    Nothing -> throwError $ "Can find & purge script : " ++ sid
    Just sRef -> do
      -- delete self
      let sp = delete sid systemST.scriptRefPool
      lift $ modifySTRef ssRef (\s -> s {scriptRefPool = sp})
      return unit
