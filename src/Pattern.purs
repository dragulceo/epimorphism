module Pattern where

import Prelude
import Data.Either (Either(..))
import Data.Either.Unsafe (fromRight)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (member, empty, lookup, insert, foldM, values, delete, StrMap())
import Data.Traversable (traverse)
import Control.Monad (unless)
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT (), lift)
import Control.Monad.ST (ST, STRef, modifySTRef, newSTRef, readSTRef)

import Config
import System (loadLib)
import Util (uuid, lg)

-- PUBLIC
checkFlag :: forall eff r. {flags :: StrMap String | r}  -> String -> Boolean
checkFlag {flags} flag = (member flag flags) && (fromJust (lookup flag flags) == "true")

-- import the modules of a pattern into the ref pool
type RData h = {mdt :: StrMap (STRef h Module), sdt :: StrMap (STRef h Script), name :: String}
importPattern :: forall eff h. STRef h (SystemST h) -> STRef h Pattern -> EpiS eff h Unit
importPattern ssRef pRef =  do
  systemST <- lift $ readSTRef ssRef
  pattern  <- lift $ readSTRef pRef

  -- import all modules
  main <- importModule ssRef (Right pattern.main)
  disp <- importModule ssRef (Right pattern.disp)
  vert <- importModule ssRef (Right pattern.vert)
  lift $ modifySTRef pRef (\p -> p {main = main, disp = disp, vert = vert})

  return unit


-- import a module into the ref pool
importModule :: forall eff h. STRef h (SystemST h) -> (Either Module String) -> EpiS eff h String
importModule ssRef md = do
  systemST <- lift $ readSTRef ssRef
  m <- case md of
    Left m -> if (checkFlag m "pool") then throwError "fuck you" else return m
    Right m -> loadLib m systemST.moduleLib "import module"

  case (checkFlag m "pool") of
    true -> return $ fromRight md
    false -> do  -- module comes from lib
      id <- lift $ uuid

      -- import children
      m' <- foldM (importChild ssRef) m m.modules
      systemST' <- lift $ readSTRef ssRef

      -- update scripts
      scripts' <- traverse (\x -> importScript ssRef (Right x) id) m.scripts
      let m'' = m' {scripts = scripts'}

      -- update pool
      let flags' = insert "pool" "true" m''.flags
      ref <- lift $ newSTRef m'' {flags = flags'}
      let mp = insert id ref systemST'.moduleRefPool  -- maybe check for duplicates here?
      lift $ modifySTRef ssRef (\s -> s {moduleRefPool = mp})

      return id

  where
    importChild :: STRef h (SystemST h) -> Module -> String -> String -> EpiS eff h Module
    importChild ssRef m k v = do
      child <- importModule ssRef (Right v)
      let modules' = insert k child m.modules
      return $ m {modules = modules'}


-- remove a module from the ref pool
purgeModule :: forall eff h. STRef h (SystemST h) -> String -> EpiS eff h Unit
purgeModule ssRef mid = do
  systemST <- lift $ readSTRef ssRef
  mRef <- loadLib mid systemST.moduleRefPool "purge module"
  mod <- lift $ readSTRef mRef

  -- purge scripts
  traverse (purgeScript ssRef) mod.scripts

  -- delete self
  let mp = delete mid systemST.moduleRefPool
  lift $ modifySTRef ssRef (\s -> s {moduleRefPool = mp})

  -- purge children
  traverse (purgeModule ssRef) (values mod.modules)

  return unit


-- replace child mn:cid(in ref pool) with child mn:c'n(in lib)
replaceModule :: forall eff h. STRef h (SystemST h) -> String -> String -> String -> (Either Module String) -> EpiS eff h Unit
replaceModule ssRef mid mn cid c' = do
  systemST <- lift $ readSTRef ssRef
  mRef <- loadLib mid systemST.moduleRefPool "replace module"
  m <- lift $ readSTRef mRef

  -- import & purge
  n' <- importModule ssRef c'
  purgeModule ssRef cid

  -- update
  let mod' = insert mn n' m.modules
  lift $ modifySTRef mRef (\m -> m {modules = mod'})

  return unit


-- import a script into the ref pool
importScript :: forall eff h. STRef h (SystemST h) -> (Either Script String) -> String -> EpiS eff h String
importScript ssRef sc mid = do
  systemST <- lift $ readSTRef ssRef
  s <- case sc of
    Left s -> return s
    Right s -> loadLib s systemST.scriptLib "import script"
  id <- lift $ uuid

  -- add module
  let s' = s {mod = Just mid}
  ref <- lift $ newSTRef s'

  --update pool
  let sp = insert id ref systemST.scriptRefPool
  lift $ modifySTRef ssRef (\s -> s {scriptRefPool = sp})

  return id


-- remove a script from the ref pool
purgeScript :: forall eff h. STRef h (SystemST h) -> String -> EpiS eff h Unit
purgeScript ssRef sid = do
  systemST <- lift $ readSTRef ssRef
  sRef <- loadLib sid systemST.scriptRefPool "purge script"

  -- delete self
  let sp = delete sid systemST.scriptRefPool
  lift $ modifySTRef ssRef (\s -> s {scriptRefPool = sp})
  return unit
