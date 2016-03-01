module Script where

import Prelude
import Data.Array (foldM, updateAt) as A
import Data.Maybe
import Data.StrMap (StrMap, keys, lookup)
import Data.Complex
import Control.Monad.ST
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)

import Config
import System (loadLib)
import Util (tLg, numFromStringE, intFromStringE)

-- PUBLIC

-- execute all scripts & script pool
runScripts :: forall eff h. Number -> StrMap (STRef h Script) -> StrMap (STRef h Module) -> EpiS eff h Unit
runScripts t slib mlib = do
  A.foldM (handle t slib mlib) unit (keys slib)
  where
    handle :: forall h eff. Number -> StrMap (STRef h Script) -> StrMap (STRef h Module) -> Unit -> String -> EpiS eff h Unit
    handle t slib mlib _ n = do
      sRef <- loadLib n slib "script"
      scr <- lift $ readSTRef sRef
      fn <- lookupScriptFN scr.fn
      case scr.mod of
        Nothing -> throwError $ "No module when running script: " ++ scr.fn
        Just mod -> fn t scr.dt mod slib mlib

-- SCRIPT FUNCTIONS

-- dont do anything.  is this necessary?
nullS :: forall h eff. ScriptFn h eff
nullS t dt mod slib mlib = do
  return unit


-- move zn[idx] around on a path
zpath :: forall h eff. ScriptFn h eff
zpath t dt mod slib mlib = do
  -- get data
  spd <- (loadLib "spd" dt "zpath spd") >>= numFromStringE
  idx <- (loadLib "idx" dt "zpath idx") >>= intFromStringE
  pathN <- loadLib "path" dt "zpath path"
  mRef <- loadLib mod mlib "zpath module"
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
