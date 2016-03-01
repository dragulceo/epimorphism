module System where

import Prelude
import Data.Array (foldM) as A
import Data.Either (Either(..))
import Data.List (fromList)
import Data.Maybe (Maybe(..))
import Data.StrMap (empty, lookup, insert, foldM, values, StrMap())
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


-- this method flattens the loaded module structure into a maps of StRefs for both modules &
-- patterns.  the idea is that we only carry around names of these things anywhere
-- and when we want one, we look it up in these pools.
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
  let mp = insert id ref systemST'.moduleRefPool
  lift $ modifySTRef ssRef (\s -> s {moduleRefPool = mp})

  return id
  where
    importChild :: STRef h (SystemST h) -> Module -> String -> String -> EpiS eff h Module
    importChild ssRef m k v = do
      child <- importModule ssRef v
      let modules' = insert k child m.modules
      return $ m {modules = modules'}


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
