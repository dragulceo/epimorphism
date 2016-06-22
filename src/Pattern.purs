module Pattern where

import Prelude
import Config (EpiS, Module, Pattern, SystemST, Script)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (STRef, modifySTRef, newSTRef, readSTRef)
import Data.Array (cons, snoc, delete, head, tail, sort) as A
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (member, StrMap, foldM, fold, delete, insert, values, lookup)
import Data.String (split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import System (loadLib)
import Util (uuid)

-- PUBLIC
-- check if an object has a flag
checkFlag :: forall r. {flags :: StrMap String | r} -> String -> String -> Boolean
checkFlag {flags} flag val = (member flag flags) && (fromJust (lookup flag flags) == val)

checkFlags :: forall r. {flags :: StrMap String | r} -> StrMap String -> Boolean
checkFlags obj flags = fold (\dt k v -> dt && checkFlag obj k v) true flags

-- filter a family by specific flags, return the keys, sorted alphabetically
flagFamily :: forall r. StrMap {flags :: StrMap String | r} -> StrMap String -> Array String
flagFamily family flags = A.sort $ fold handle [] family
  where
    handle res k v = case (checkFlags v flags) of
      true -> A.snoc res k
      false -> res


-- find a module given an address - ie main.main_body.t
findModule :: forall eff h. StrMap (STRef h Module) -> Pattern -> String -> Boolean -> EpiS eff h String
findModule mpool pattern dt followSwitch = do
  let addr = split "." dt
  case (A.head addr) of
    Nothing -> throwError "we need data, chump"
    Just "vert" -> findModule' mpool pattern.vert (fromJust $ A.tail addr) followSwitch
    Just "disp" -> findModule' mpool pattern.disp (fromJust $ A.tail addr) followSwitch
    Just "main" -> findModule' mpool pattern.main (fromJust $ A.tail addr) followSwitch
    Just x      -> throwError $ "value should be main, vert, or disp : " ++ x


findModule' :: forall eff h. StrMap (STRef h Module) -> String -> Array String -> Boolean -> EpiS eff h String
findModule' mpool mid addr followSwitch = do

  maybe (return $ mid) handle (A.head addr)
  where
    handle mid' = do
      mRef    <- loadLib mid mpool "findModule'"
      mod     <- lift $ readSTRef mRef
      childId <- loadLib mid' mod.modules "findModule' find child"
      cRef    <- loadLib childId mpool "findModule' child ref"
      child   <- lift $ readSTRef cRef
      addr'   <- return $ fromJust $ A.tail addr

      case (checkFlag child "family" "switch" && followSwitch) of
        true ->  findModule' mpool childId (A.cons "m1" addr') followSwitch
        false -> findModule' mpool childId addr' followSwitch



-- find parent module id & submodule that a module is binded to. kind of ghetto
findParent :: forall eff h. StrMap (STRef h Module) -> String -> EpiS eff h (Tuple String String)
findParent mpool mid = do
  res <- foldM handle Nothing mpool
  case res of
    Nothing -> throwError $ "module has no parent: " ++ mid
    Just x -> return x
  where
    handle :: Maybe (Tuple String String) -> String -> STRef h Module -> EpiS eff h (Maybe (Tuple String String))
    handle (Just x) _ _ = return $ Just x
    handle _ pid ref = do
      mod <- lift $ readSTRef ref
      case (fold handle2 Nothing mod.modules) of
        Nothing -> return Nothing
        Just x -> do
          return $ Just $ Tuple pid x
    handle2 :: Maybe String -> String -> String -> Maybe String
    handle2 (Just x) _ _ = Just x
    handle2 _ k cid | cid == mid = Just k
    handle2 _ _ _ = Nothing


-- IMPORTING ---

-- import the modules of a pattern into the ref pool
data ImportObj = ImportModule Module | ImportScript Script | ImportRef String
importPattern :: forall eff h. STRef h (SystemST h) -> STRef h Pattern -> EpiS eff h Unit
importPattern ssRef pRef =  do
  systemST <- lift $ readSTRef ssRef
  pattern  <- lift $ readSTRef pRef

  -- import all modules
  main <- importModule ssRef (ImportRef pattern.main)
  disp <- importModule ssRef (ImportRef pattern.disp)
  vert <- importModule ssRef (ImportRef pattern.vert)
  lift $ modifySTRef pRef (\p -> p {main = main, disp = disp, vert = vert})

  return unit


-- import a module into the ref pool
importModule :: forall eff h. STRef h (SystemST h) -> ImportObj -> EpiS eff h String
importModule ssRef obj = do
  systemST <- lift $ readSTRef ssRef
  id <- lift $ uuid

  -- find module
  mod <- case obj of
    ImportModule m -> return m
    ImportRef n -> case (member n systemST.moduleRefPool) of
      true -> do
        ref <- loadLib n systemST.moduleRefPool "reimport from pool"
        lift $ readSTRef ref
      false -> do
        loadLib n systemST.moduleLib "import module lib"
    ImportScript _ -> throwError "dont give me a script"

  -- update pool
  ref <- lift $ newSTRef mod
  let mp' = insert id ref systemST.moduleRefPool  -- maybe check for duplicates here?
  lift $ modifySTRef ssRef (\s -> s {moduleRefPool = mp'})

  -- import children
  foldM (importChild ssRef) id mod.modules

  -- update scripts
  traverse (\x -> importScript ssRef (ImportRef x) id) mod.scripts

  return id

  where
    importChild :: STRef h (SystemST h) -> String -> String -> String -> EpiS eff h String
    importChild ssRef mid k v = do
      systemSTC <- lift $ readSTRef ssRef
      systemST <- lift $ readSTRef ssRef

      -- import child
      child <- importModule ssRef (ImportRef v)

      -- update parent
      mRef <- loadLib mid systemSTC.moduleRefPool "import module - update parent"
      m <- lift $ readSTRef mRef
      let modules' = insert k child m.modules
      lift $ modifySTRef mRef (\m' -> m' {modules = modules'})

      return mid


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


-- replace child subN:cid(in ref pool) with child subN:obj
replaceModule :: forall eff h. STRef h (SystemST h) -> String -> String -> String -> ImportObj -> EpiS eff h String
replaceModule ssRef mid subN cid obj = do
  systemST <- lift $ readSTRef ssRef
  mRef <- loadLib mid systemST.moduleRefPool "replace module"
  m <- lift $ readSTRef mRef

  -- import & purge
  n' <- importModule ssRef obj
  purgeModule ssRef cid

  -- update
  let mod' = insert subN n' m.modules
  lift $ modifySTRef mRef (\m' -> m' {modules = mod'})

  return n'


-- import a script into the ref pool
importScript :: forall eff h. STRef h (SystemST h) -> ImportObj -> String -> EpiS eff h String
importScript ssRef obj mid = do
  systemST <- lift $ readSTRef ssRef
  id <- lift $ uuid
  mRef <- loadLib mid systemST.moduleRefPool "import script - find module"
  m <- lift $ readSTRef mRef

  scr <- case obj of
    ImportScript s -> do
      let tPhase' = systemST.t - s.tPhase -- UPDATE PHASE
      return $ s {tPhase = tPhase'}
    ImportRef n -> do
      let scripts' = A.delete n m.scripts
      lift $ modifySTRef mRef (\m' -> m' {scripts = scripts'})

      case (member n systemST.scriptRefPool) of
        true -> do
          ref <- loadLib n systemST.scriptRefPool "import script pool"
          lift $ readSTRef ref
        false -> do
          scr' <- loadLib n systemST.scriptLib "import script"
          let tPhase' = systemST.t - scr'.tPhase
          return $ scr' {tPhase = tPhase'}
    ImportModule _ -> throwError "dont give me a module"

  --update pool
  ref <- lift $ newSTRef scr {mid = mid}
  let pool' = insert id ref systemST.scriptRefPool
  lift $ modifySTRef ssRef (\s' -> s' {scriptRefPool = pool'})

  -- add script to module
  m' <- lift $ readSTRef mRef
  let scripts' = A.snoc m'.scripts id
  lift $ modifySTRef mRef (\m'' -> m'' {scripts = scripts'})

  return id


-- remove a script from the ref pool
purgeScript :: forall eff h. STRef h (SystemST h) -> String -> EpiS eff h Unit
purgeScript ssRef sid = do
  systemST <- lift $ readSTRef ssRef
  sRef <- loadLib sid systemST.scriptRefPool "purge script - find script"
  sc <- lift $ readSTRef sRef

  -- delete self
  let sp = delete sid systemST.scriptRefPool
  lift $ modifySTRef ssRef (\s -> s {scriptRefPool = sp})

  -- remove from module
  case sc.mid of
    "" -> throwError $ "wtf didn't this script have a module: " ++ sid
    mid -> do
      mRef <- loadLib mid systemST.moduleRefPool "purge script - find module"
      mod <- lift $ readSTRef mRef
      lift $ modifySTRef mRef (\m -> m {scripts = A.delete sid mod.scripts})

  return unit
