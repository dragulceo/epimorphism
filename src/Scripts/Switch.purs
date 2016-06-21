module Switch where

import Prelude
import Config (ScriptFn, EpiS, SystemST)
import Control.Monad (when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (STRef, readSTRef)
import Data.Array (index, length, null) as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(Just, Nothing))
import Data.StrMap (fromFoldable, insert, member, union)
import Data.Tuple (Tuple(..))
import Pattern (replaceModule, findParent, importModule, purgeScript, flagFamily)
import ScriptUtil (createScript)
import System (loadLib)
import Util (lg, numFromStringE, intFromStringE, gmod)

-- increment a module within the specified families
incMod :: forall eff h. ScriptFn eff h
incMod ssRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef
  let dt = scr.dt

  -- get data
  idx   <- (loadLib "idx" dt "incMod idx") >>= intFromStringE
  spd   <- (loadLib "spd" dt "incMod spd") >>= numFromStringE
  subN  <- loadLib "sub" dt "incMod sub"
  dim   <- loadLib "dim" dt "incMod dim"
  lib   <- loadLib "lib" dt "incMod lib"

  -- index & next data
  let index = flagFamily systemST.moduleLib $ fromFoldable [(Tuple "family" subN), (Tuple lib "true")]

  when (A.null index) do
    throwError $ "your index doesnt exist"

  let nxtPos = idx `gmod` (A.length index)

  m1 <- case (A.index index nxtPos) of
    Nothing -> throwError $ "your index doesnt exist" -- doesn't look like this works
    Just v -> return v

  let nul = lg $ "SWITCHING : " ++ mid ++ ":" ++ subN ++ " to : " ++ m1

  switchModules ssRef mid subN m1 dim spd t

  -- remove self
  purgeScript ssRef self

  return true


-- increment a substitution through an index
incSub :: forall eff h. ScriptFn eff h
incSub ssRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef
  let dt = scr.dt

  -- get data
  idx   <- (loadLib "idx" dt "incSub idx") >>= intFromStringE
  spd   <- (loadLib "spd" dt "incSub spd") >>= numFromStringE
  subN  <- loadLib "sub" dt "incSub sub"
  dim   <- loadLib "dim" dt "incMod dim"
  lib   <- loadLib "lib" dt "incSub ind"

  index <- loadLib lib systemST.indexLib "incSub index"

  when (A.null index.lib) do
    throwError $ "your index doesnt exist"

  let nxtPos = idx `gmod` (A.length index.lib)

  sub <- case (A.index index.lib nxtPos) of
    Nothing -> throwError $ "your index doesnt exist" -- doesn't look like this works
    Just v -> return v

  let nul = lg $ "SWITCHING : " ++ mid ++ ":" ++ subN ++ " to : " ++ sub

  -- remove self (do this before duplicating module)
  purgeScript ssRef self

  -- duplicate & switch
  mRef  <- loadLib mid systemST.moduleRefPool "incSub module"
  m     <- lift $ readSTRef mRef
  case (member subN m.sub) of
    true -> do
      let sub' = insert subN sub m.sub
      let flags' = insert "pool" "false" m.flags
      let m' = m {sub = sub', flags = flags'}
      m'id <- importModule ssRef (Left m') -- this is kind of hackish, as its reimported

      (Tuple parent subN') <- findParent systemST.moduleRefPool mid
      switchModules ssRef parent subN' m'id dim spd t
      return true
    false -> do
      let nul' = lg "TEMP: can't find sub!"
      return false


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

  swid <- replaceModule ssRef mid subN m0 (Left switch')

  -- create & import blending script
  createScript ssRef swid "default" "finishSwitch" $ fromFoldable [(Tuple "spd" (show spd))]
  createScript ssRef swid "default" "ppath" $ fromFoldable [(Tuple "par" "intrp"), (Tuple "path" "linear"), (Tuple "spd" (show spd))]

  return unit


finishSwitch :: forall eff h. ScriptFn eff h
finishSwitch ssRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef
  let dt = scr.dt

  -- get data
  spd  <- (loadLib "spd" dt "finishSwitch spd") >>= numFromStringE

  case t * spd of
    -- we're done
    x | x >= 1.0 -> do
      let a = lg "DONE SWITCHING"

      -- find parent & m1
      (Tuple parent subN) <- findParent systemST.moduleRefPool mid
      mRef   <- loadLib mid systemST.moduleRefPool "finishSwitch module"
      m      <- lift $ readSTRef mRef
      m1     <- loadLib "m1" m.modules "finishSwitch module"

      -- replace.  this removes all scripts wrt this as well
      replaceModule ssRef parent subN mid (Right m1)

      return true
    _ -> do
      return false
