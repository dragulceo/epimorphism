module Script where

import Prelude
import Data.Array (index, length, elemIndex, foldM, updateAt, null) as A
import Data.Complex
import Data.Either (Either(..))
import Data.Foldable (or)
import Data.Maybe
import Data.StrMap (StrMap, keys, lookup, insert, fromFoldable, union, member, delete)
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Control.Monad (unless, when)
import Control.Monad.ST
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)

import Math (pi, cos, floor)

import Config
import System (loadLib)
import Util (lg, tLg, numFromStringE, intFromStringE, gmod, rndstr, randInt)
import Pattern (purgeScript, replaceModule, importScript, flagFamily, findParent, importModule, findModule')

-- PUBLIC

-- execute all scripts & script pool
runScripts :: forall eff h. STRef h (SystemST h) -> EpiS eff h Boolean
runScripts ssRef = do
  systemST <- lift $ readSTRef ssRef
  res <- traverse (handle ssRef) (keys systemST.scriptRefPool)
  return $ or res
  where
    handle :: forall eff h. STRef h (SystemST h) -> String -> EpiS eff h Boolean
    handle ssRef n = do
      systemST <- lift $ readSTRef ssRef
      case (member n systemST.scriptRefPool) of
        true -> do
          sRef <- loadLib n systemST.scriptRefPool "runScripts"
          scr <- lift $ readSTRef sRef
          fn <- lookupScriptFN scr.fn
          let t' = systemST.t - scr.tPhase
          case scr.mid of
            Nothing -> throwError $ "No module when running script: " ++ scr.fn
            Just mid -> fn ssRef n t' mid sRef
        false -> do
          let g = lg "script removed" -- ghetto
          return false

-- SCRIPT FUNCTIONS

-- dont do anything.  is this necessary?
nullS :: forall eff h. ScriptFn eff h
nullS ssRef self t mid sRef = do
  return false


-- move par[par] around on a path
ppath :: forall eff h. ScriptFn eff h
ppath ssRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef
  let dt = scr.dt

  -- get data
  spd <- (loadLib "spd" dt "ppath spd") >>= numFromStringE
  par <- loadLib "par" dt "ppath par"
  pathN <- loadLib "path" dt "ppath path"
  mRef <- loadLib mid systemST.moduleRefPool "ppath module"
  m <- lift $ readSTRef mRef

  -- lookup path function
  fn <- case pathN of
    "linear" -> do
      return $ \t -> t
    "loop" -> do
      return $ \t -> t - floor(t)
    "wave" -> do
      a <- (loadLib "a" dt "ppath linear a") >>= numFromStringE
      b <- (loadLib "b" dt "ppath linear b") >>= numFromStringE
      return $ \t -> a * cos(t) + b
    _ -> throwError $ "Unknown par path : " ++ pathN

  -- execute
  let val = fn (t * spd)

  -- modify data
  let par' = insert par val m.par
  lift $ modifySTRef mRef (\m -> m {par = par'})

  return false


-- move zn[idx] around on a path
zpath :: forall eff h. ScriptFn eff h
zpath ssRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef
  let dt = scr.dt

  -- get data
  spd <- (loadLib "spd" dt "zpath spd") >>= numFromStringE
  idx <- (loadLib "idx" dt "zpath idx") >>= intFromStringE
  pathN <- loadLib "path" dt "zpath path"
  mRef <- loadLib mid systemST.moduleRefPool "zpath module"
  m <- lift $ readSTRef mRef

  -- lookup path function
  fn <- case pathN of
    "linx" -> return $ \t ->
      outCartesian $ Cartesian t 0.0
    "liny" -> return $ \t ->
      outCartesian $ Cartesian 0.0 t
    "circle" -> do
      r <- (loadLib "r" dt "zpath circle r") >>= numFromStringE
      return $ \t ->
        outPolar $ Polar (2.0 * pi * t) r
    "rose" -> do
      a <- (loadLib "a" dt "zpath rose a") >>= numFromStringE
      b <- (loadLib "b" dt "zpath rose b") >>= numFromStringE
      c <- (loadLib "c" dt "zpath rose c") >>= numFromStringE
      return $ \t ->
        outPolar $ Polar (2.0 * pi * t) (a * cos(b * t) + c)
    _ -> throwError $ "Unknown z path : " ++ pathN

  -- execute
  let z' = fn (t * spd)

  -- modify data
  case (A.updateAt idx z' m.zn) of
    (Just zn') -> lift $ modifySTRef mRef (\m -> m {zn = zn'})
    _ -> throwError $ "zn idx out of bound : " ++ (show idx) ++ " : in zpath"

  return false


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
      let nul = lg "TEMP: can't find sub!"
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


randomMain :: forall eff h. ScriptFn eff h
randomMain ssRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  scr' <- lift $ readSTRef sRef

  unless(member "nxt" scr'.dt) do
    let dt' = insert "nxt" (show (t + 5.0)) scr'.dt
    lift $ modifySTRef sRef (\s -> s {dt = dt'})
    return unit

  scr <- lift $ readSTRef sRef
  let dt = scr.dt

  nxt <- (loadLib "nxt" dt "randomMain nxt") >>= numFromStringE

  case t of
    t | t > nxt -> do
      let a = lg "do thing"
      let dt' = insert "nxt" (show (t + 5.0)) dt
      lift $ modifySTRef sRef (\s -> s {dt = dt'})

      idx <- lift $ randInt 100
      tmid <- findModule' systemST.moduleRefPool systemST.mainRef ["main_body", "t"]

      createScript ssRef tmid "default" "incSub" $ fromFoldable [(Tuple "sub" "t_inner"), (Tuple "idx" (show idx)), (Tuple "spd" "0.15"), (Tuple "lib" "t_inner"), (Tuple "dim" "vec2")]

      return false
    _ -> return false


-- PRIVATE

-- find script fuction given name
lookupScriptFN :: forall eff h. String -> EpiS eff h (ScriptFn eff h)
lookupScriptFN n = case n of
  "null"   -> return nullS
  "ppath"  -> return ppath
  "zpath"  -> return zpath
  "incMod" -> return incMod
  "incSub" -> return incSub
  "finishSwitch" -> return finishSwitch
  "randomMain" -> return randomMain
  _       -> throwError $ "script function not found: " ++ n


-- create a script dynamically & import it
createScript :: forall eff h. STRef h (SystemST h) -> String -> String -> String -> StrMap String -> EpiS eff h String
createScript ssRef mid parent fn dt = do
  systemST <- lift $ readSTRef ssRef
  scr      <- loadLib parent systemST.scriptLib "create script"

  let scr' = scr {fn = fn, dt = union dt scr.dt}
  importScript ssRef (Left scr') mid
