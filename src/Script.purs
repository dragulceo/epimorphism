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

lookupScriptFN :: forall eff h. String -> Epi (st :: ST h | eff) (ScriptFn h eff)
lookupScriptFN n = case n of
  "null" -> return nullS
  "zpath" -> return zpath
  _ -> throwError $ "script function not found: " ++ n


runScripts :: forall eff h. Number -> StrMap (STRef h Script) -> StrMap (STRef h Module) -> Epi (st :: ST h | eff) Unit
runScripts t slib mlib = do
  A.foldM (handle t slib mlib) unit (keys slib)
  where
    handle :: forall h eff. Number -> StrMap (STRef h Script) -> StrMap (STRef h Module) -> Unit -> String -> Epi (st :: ST h | eff) Unit
    handle t slib mlib _ n = do
      sRef <- loadLib n slib
      scr <- lift $ readSTRef sRef
      fn <- lookupScriptFN scr.fn
      fn t scr.dt scr.mod slib mlib


nullS :: forall h eff. ScriptFn h eff
nullS t dt mod slib mlib = do
  return unit


-- inline this
type ZPathFn = Number -> Complex
lookupZPathFn :: forall eff. String -> Epi eff ZPathFn
lookupZPathFn n = case n of
  "rlin" -> return $ \t ->
    outCartesian $ Cartesian t 0.0
  "circle" -> return $ \t ->
    outPolar $ Polar t 1.0
  _ -> throwError $ "Unknown z path : " ++ n


zpath :: forall h eff. ScriptFn h eff
zpath t dt mod slib mlib = do
  spd <- (loadLib "spd" dt) >>= numFromStringE
  idx <- (loadLib "idx" dt) >>= intFromStringE
  pathN <- loadLib "path" dt
  fn <- lookupZPathFn pathN
  mRef <- loadLib mod mlib
  m <- lift $ readSTRef mRef

  let z' = fn (t * spd)
  case (A.updateAt idx z' m.zn) of
    (Just zn') -> lift $ modifySTRef mRef (\m -> m {zn = zn'})
    _ -> throwError $ "zn idx out of bound : " ++ (show idx) ++ " : in zpath"

  return unit
