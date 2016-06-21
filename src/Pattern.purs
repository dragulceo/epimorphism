module Pattern where

import Prelude
import Data.Array (snoc, delete, head, tail, sort) as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (StrMap, foldM, fold, delete, insert, member, values, lookup)
import Data.String (split)
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (STRef, modifySTRef, newSTRef, readSTRef)

import Config (EpiS, Module, Pattern, SystemST, Script)
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


-- import the modules of a pattern into the ref pool
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
data ImportObj = ImportModule Module | ImportRef String | ImportLib String
importModule :: forall eff h. STRef h (SystemST h) -> (Either Module String) -> EpiS eff h String
importModule ssRef md = do
  systemST <- lift $ readSTRef ssRef
  id <- lift $ uuid

  -- find module
  m <- case md of
    Left m'' -> do -- we're given a module
      if (checkFlag m'' "pool" "true") then throwError "fuck you" else return m''
    Right m' -> do -- we're given a name (lib or pool ref)
      case (member m' systemST.moduleRefPool) of
        true -> do
          ref <- loadLib m' systemST.moduleRefPool "import module pool"
          lift $ readSTRef ref
        false -> do
          loadLib m' systemST.moduleLib "import module lib"

  -- update pool
  let flags' = insert "pool" "true" m.flags
  ref <- lift $ newSTRef m {flags = flags'}
  let mp' = insert id ref systemST.moduleRefPool  -- maybe check for duplicates here?
  lift $ modifySTRef ssRef (\s -> s {moduleRefPool = mp'})

  -- import children
  foldM (importChild ssRef) id m.modules

  -- update scripts
  traverse (\x -> importScript ssRef (Right x) id) m.scripts

  return id

  where
    importChild :: STRef h (SystemST h) -> String -> String -> String -> EpiS eff h String
    importChild ssRef mid k v = do
      systemSTC <- lift $ readSTRef ssRef
      -- import child
      child <- importModule ssRef (Right v)

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


-- replace child subN:cid(in ref pool) with child subN:c'(in lib)
replaceModule :: forall eff h. STRef h (SystemST h) -> String -> String -> String -> (Either Module String) -> EpiS eff h String
replaceModule ssRef mid subN cid c' = do
  systemST <- lift $ readSTRef ssRef
  mRef <- loadLib mid systemST.moduleRefPool "replace module"
  m <- lift $ readSTRef mRef

  -- import & purge
  n' <- importModule ssRef c'
  purgeModule ssRef cid

  -- update
  let mod' = insert subN n' m.modules
  lift $ modifySTRef mRef (\m' -> m' {modules = mod'})

  return n'


-- import a script into the ref pool
importScript :: forall eff h. STRef h (SystemST h) -> (Either Script String) -> String -> EpiS eff h String
importScript ssRef sc mid = do
  systemST <- lift $ readSTRef ssRef
  id <- lift $ uuid
  mRef <- loadLib mid systemST.moduleRefPool "import script - find module"

  s <- case sc of
    Left s' -> do
      let tPhase' = systemST.t - s'.tPhase
      return $ s' {tPhase = tPhase'}
    Right s' -> do
      m <- lift $ readSTRef mRef
      let scripts' = A.delete s' m.scripts
      lift $ modifySTRef mRef (\m' -> m' {scripts = scripts'})

      case (member s' systemST.scriptRefPool) of
        true -> do
          ref <- loadLib s' systemST.scriptRefPool "import script pool"
          lift $ readSTRef ref
        false -> do
          scr <- loadLib s' systemST.scriptLib "import script"
          let tPhase' = systemST.t - scr.tPhase
          return $ scr {tPhase = tPhase'}

  --update pool
  ref <- lift $ newSTRef s {mid = mid}
  let sp = insert id ref systemST.scriptRefPool
  lift $ modifySTRef ssRef (\s' -> s' {scriptRefPool = sp})

  -- add script
  m <- lift $ readSTRef mRef
  let scripts' = A.snoc m.scripts id
  lift $ modifySTRef mRef (\m' -> m' {scripts = scripts'})

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


-- find a module given an address - ie main.main_body.t
findModule :: forall eff h. StrMap (STRef h Module) -> Pattern -> String -> EpiS eff h String
findModule mpool pattern dt = do
  let addr = split "." dt
  case (A.head addr) of
    Nothing -> throwError "we need data, chump"
    Just "vert" -> findModule' mpool pattern.vert $ fromJust $ A.tail addr
    Just "disp" -> findModule' mpool pattern.disp $ fromJust $ A.tail addr
    Just "main" -> findModule' mpool pattern.main $ fromJust $ A.tail addr
    Just x      -> throwError $ "value should be main, vert, or disp : " ++ x


findModule' :: forall eff h. StrMap (STRef h Module) -> String -> Array String -> EpiS eff h String
findModule' mpool mid addr = do
  maybe (return $ mid) handle (A.head addr)
  where
    handle mid' = do
      mRef <- loadLib mid mpool "findModule'"
      mod <- lift $ readSTRef mRef
      c <- loadLib mid' mod.modules "findModule' find child"
      findModule' mpool c $ fromJust $ A.tail addr


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
