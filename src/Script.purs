module Script where

import Prelude
import Data.Array (foldM, updateAt) as A
import Data.Maybe
import Data.StrMap (StrMap, keys, lookup)
import Data.Traversable (traverse)
import Data.Complex
import Control.Monad.ST
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)

import Config
import System (loadLib)
import Util (tLg, numFromStringE, intFromStringE)

-- PUBLIC

-- execute all scripts & script pool
runScripts :: forall eff h. STRef h (SystemST h) -> Number -> EpiS eff h Unit
runScripts ssRef t = do
  systemST <- lift $ readSTRef ssRef
  traverse (handle ssRef t) (keys systemST.scriptRefPool)
  return unit
  where
    handle :: forall h eff. STRef h (SystemST h) -> Number -> String -> EpiS eff h Unit
    handle ssRef t n = do
      systemST <- lift $ readSTRef ssRef
      sRef <- loadLib n systemST.scriptRefPool "script"
      scr <- lift $ readSTRef sRef
      fn <- lookupScriptFN scr.fn
      case scr.mod of
        Nothing -> throwError $ "No module when running script: " ++ scr.fn
        Just mod -> fn ssRef t scr.dt mod
      return unit

-- SCRIPT FUNCTIONS

-- dont do anything.  is this necessary?
nullS :: forall h eff. ScriptFn h eff
nullS ssRef t dt mod = do
  return unit


-- move zn[idx] around on a path
zpath :: forall h eff. ScriptFn h eff
zpath ssRef t dt mod = do
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

  return unit


-- PRIVATE

-- find script fuction given name
lookupScriptFN :: forall eff h. String -> EpiS eff h (ScriptFn eff h)
lookupScriptFN n = case n of
  "null"  -> return nullS
  "zpath" -> return zpath
  _       -> throwError $ "script function not found: " ++ n
