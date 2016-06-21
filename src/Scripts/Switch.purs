module Switch where

import Prelude
import Config (Script, ScriptFn, EpiS, SystemST)
import Control.Monad (when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (STRef, readSTRef)
import Data.Array (index, length, null, updateAt) as A
import Data.Maybe (Maybe(Just, Nothing))
import Data.StrMap (fromFoldable, insert, member, union)
import Data.Tuple (Tuple(..))
import Pattern (ImportObj(ImportModule), replaceModule, findParent, importModule, purgeScript, flagFamily)
import ScriptUtil (createScript)
import System (loadLib)
import Util (lg, numFromStringE, intFromStringE, gmod)

incData :: forall eff h. SystemST h -> Script -> (String -> String -> EpiS eff h (Array String)) -> EpiS eff h {sub :: String, nxt :: String, dim :: String, spd :: Number}
incData systemST scr loader = do
  let dt = scr.dt

  -- get data
  idx   <- (loadLib "idx" dt "incMod idx") >>= intFromStringE
  spd   <- (loadLib "spd" dt "incMod spd") >>= numFromStringE
  sub   <- loadLib "sub" dt "incMod sub"
  dim   <- loadLib "dim" dt "incMod dim"
  lib   <- loadLib "lib" dt "incMod lib"

  -- index & next data
  index <- loader lib sub

  when (A.null index) do
    throwError $ "your index doesnt exist"

  let nxtPos = idx `gmod` (A.length index)

  nxt <- case (A.index index nxtPos) of
    Nothing -> throwError $ "your index doesnt exist" -- doesn't look like this works
    Just v -> return v

  return {sub, nxt, dim, spd}


-- increment a module within the specified families
incMod :: forall eff h. ScriptFn eff h
incMod ssRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef

  {sub, nxt, dim, spd} <- incData systemST scr
    \l' s' -> return $ flagFamily systemST.moduleLib $ fromFoldable [(Tuple "family" s'), (Tuple l' "true")]

  let nul = lg $ "SWITCHING : " ++ mid ++ ":" ++ sub ++ " to : " ++ nxt

  switchModules ssRef mid sub nxt dim spd t

  -- remove self
  purgeScript ssRef self

  return true


-- increment a substitution through an index
incSub :: forall eff h. ScriptFn eff h
incSub ssRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef

  {sub, nxt, dim, spd} <- incData systemST scr
    \l' s' -> do
      dt <- loadLib l' systemST.indexLib "incSub index"
      return dt.lib

  let nul = lg $ "SWITCHING : " ++ mid ++ ":" ++ sub ++ " to : " ++ nxt

  -- remove self (do this before duplicating module)
  purgeScript ssRef self

  -- duplicate & switch
  mRef  <- loadLib mid systemST.moduleRefPool "incSub module"
  m     <- lift $ readSTRef mRef
  case (member sub m.sub) of
    true -> do
      let sub' = insert sub nxt m.sub
      let m' = m {sub = sub'}
      m'id <- importModule ssRef (ImportModule m') -- this is kind of hackish, as its reimported

      (Tuple parent child) <- findParent systemST.moduleRefPool mid
      switchModules ssRef parent child m'id dim spd t
      return true
    false -> do  -- HRM, I think we can do better here
      let nul' = lg "TEMP: can't find sub!"
      return false


incImage :: forall eff h. ScriptFn eff h
incImage ssRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef

  {sub, nxt, dim, spd} <- incData systemST scr
    \l' s' -> do
      dt <- loadLib l' systemST.indexLib "incImage index"
      return dt.lib

  let nul = lg $ "SWITCHING : " ++ mid ++ ":" ++ sub ++ " to : " ++ nxt

  -- remove self (do this before duplicating module)
  purgeScript ssRef self

  -- duplicate & switch
  mRef  <- loadLib mid systemST.moduleRefPool "incImage module"
  m     <- lift $ readSTRef mRef
  idx   <- intFromStringE sub
  case (A.updateAt idx nxt m.images) of
    Just images' -> do
      let m' = m {images = images'}
      m'id <- importModule ssRef (ImportModule m') -- this is kind of hackish, as its reimported

      (Tuple parent childN) <- findParent systemST.moduleRefPool mid
      switchModules ssRef parent childN m'id dim spd t
      return true
    Nothing -> do  -- HRM, maybe we want to be able to expand the number of existing images
      let nul' = lg "TEMP: don't have enough images!"
      return false

  return true


switchModules :: forall eff h. STRef h (SystemST h) -> String -> String -> String -> String -> Number -> Number -> EpiS eff h Unit
switchModules ssRef mid subN m1 dim spd t = do
  systemST <- lift $ readSTRef ssRef
  mRef  <- loadLib mid systemST.moduleRefPool "incStd module"
  m     <- lift $ readSTRef mRef
  m0    <- loadLib subN m.modules "incStd find sub"
  m0Ref <- loadLib m0 systemST.moduleRefPool "incStd m0"
  m0M   <- lift $ readSTRef m0Ref

  -- create switch module
  switch <- loadLib "smooth_switch" systemST.moduleLib "switchModules"

  let modules = fromFoldable [(Tuple "m0" m0), (Tuple "m1" m1)]
  let sub'    = union (fromFoldable [(Tuple "dim" dim), (Tuple "var" m0M.var)]) switch.sub
  let switch' = switch {sub = sub', modules = modules, var = m0M.var}

  swid <- replaceModule ssRef mid subN m0 (ImportModule switch')

  -- create & import blending script
  createScript ssRef swid "default" "finishSwitch" $ fromFoldable [(Tuple "delay" (show spd))]
  createScript ssRef swid "default" "ppath" $ fromFoldable [(Tuple "par" "intrp"), (Tuple "path" "linear"), (Tuple "spd" (show spd))]

  return unit


finishSwitch :: forall eff h. ScriptFn eff h
finishSwitch ssRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef
  let dt = scr.dt

  -- get data
  delay <- (loadLib "delay" dt "finishSwitch delay") >>= numFromStringE

  case t * delay of
    -- we're done
    x | x >= 1.0 -> do
      let a = lg "DONE SWITCHING"

      -- find parent & m1
      (Tuple parent subN) <- findParent systemST.moduleRefPool mid
      mRef   <- loadLib mid systemST.moduleRefPool "finishSwitch module"
      m      <- lift $ readSTRef mRef
      m1id   <- loadLib "m1" m.modules "finishSwitch module"
      m1Ref  <- loadLib m1id systemST.moduleRefPool "finishSwitch m1"
      m1     <- lift $ readSTRef m1Ref

      -- replace.  this removes all scripts wrt this as well
      replaceModule ssRef parent subN mid (ImportModule m1)

      return true
    _ -> do
      return false
