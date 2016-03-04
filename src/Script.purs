module Script where

import Prelude
import Data.Array (index, length, elemIndex, foldM, updateAt) as A
import Data.Complex
import Data.Foldable (or)
import Data.Maybe
import Data.StrMap (StrMap, keys, lookup, insert)
import Data.String (trim)
import Data.Traversable (traverse)
import Control.Monad.ST
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)

import Config
import System (loadLib)
import Util (tLg, numFromStringE, intFromStringE, gmod)
import Pattern (purgeScript, replaceModule)

-- PUBLIC

-- execute all scripts & script pool
runScripts :: forall eff h. STRef h (SystemST h) -> Number -> EpiS eff h Boolean
runScripts ssRef t = do
  systemST <- lift $ readSTRef ssRef
  res <- traverse (handle ssRef t) (keys systemST.scriptRefPool)
  return $ or res
  where
    handle :: forall h eff. STRef h (SystemST h) -> Number -> String -> EpiS eff h Boolean
    handle ssRef t n = do
      systemST <- lift $ readSTRef ssRef
      sRef <- loadLib n systemST.scriptRefPool "script"
      scr <- lift $ readSTRef sRef
      fn <- lookupScriptFN scr.fn
      case scr.mod of
        Nothing -> throwError $ "No module when running script: " ++ scr.fn
        Just mod -> fn ssRef n t scr.dt mod

-- SCRIPT FUNCTIONS

-- dont do anything.  is this necessary?
nullS :: forall h eff. ScriptFn h eff
nullS ssRef self t dt mod = do
  return false


-- move zn[idx] around on a path
zpath :: forall h eff. ScriptFn h eff
zpath ssRef self t dt mod = do
  systemST <- lift $ readSTRef ssRef

  -- get data
  spd <- (loadLib "spd" dt "zpath spd") >>= numFromStringE
  idx <- (loadLib "idx" dt "zpath idx") >>= intFromStringE
  pathN <- loadLib "path" dt "zpath path"
  mRef <- loadLib mod systemST.moduleRefPool "zpath module"
  m <- lift $ readSTRef mRef

  -- lookup path function
  fn <- case pathN of
    "rlin" -> return $ \t ->
      outCartesian $ Cartesian t 0.0
    "circle" -> return $ \t ->
      outPolar $ Polar t 1.0
    _ -> throwError $ "Unknown z path : " ++ pathN

  -- execute
  let z' = fn (t * spd)

  -- modify data
  case (A.updateAt idx z' m.zn) of
    (Just zn') -> lift $ modifySTRef mRef (\m -> m {zn = zn'})
    _ -> throwError $ "zn idx out of bound : " ++ (show idx) ++ " : in zpath"

  return false


-- increment a substitution by looking through the index
incIdx :: forall h eff. ScriptFn h eff
incIdx ssRef self t dt mid = do
  systemST <- lift $ readSTRef ssRef

  -- get data
  inc <- (loadLib "inc" dt "incIdx inc") >>= intFromStringE
  subN <- loadLib "sub" dt "incIdx sub"
  mRef <- loadLib mid systemST.moduleRefPool "incIdx module"
  m <- lift $ readSTRef mRef

  sub <- loadLib subN m.sub "incIdx find sub"

  -- index
  index <- loadLib subN systemST.indexLib "incIdx index"
  nxtPos <- case (A.elemIndex sub index.lib) of
    Nothing -> return 3
    Just i -> return $ (i + inc) `gmod` (A.length index.lib)

  nxtVal <- case (A.index index.lib nxtPos) of
    Nothing -> throwError $ "elem out of bound : " ++ (show nxtPos) ++ " : " ++ subN
    Just v -> return v

  -- do thing
  let sub' = insert subN nxtVal m.sub
  lift $ modifySTRef mRef (\m -> m {sub = sub'})

  -- rebuild

  -- remove self
  purgeScript ssRef self

  return true


-- PRIVATE

-- find script fuction given name
lookupScriptFN :: forall eff h. String -> EpiS eff h (ScriptFn eff h)
lookupScriptFN n = case n of
  "null"  -> return nullS
  "zpath" -> return zpath
  "incIdx" -> return incIdx
  _       -> throwError $ "script function not found: " ++ n
