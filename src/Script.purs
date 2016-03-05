module Script where

import Prelude
import Data.Array (index, length, elemIndex, foldM, updateAt) as A
import Data.Complex
import Data.Either (Either(..))
import Data.Foldable (or)
import Data.Maybe
import Data.StrMap (StrMap, keys, lookup, insert, fromFoldable, union, member, delete)
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Control.Monad (unless)
import Control.Monad.ST
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)

import Config
import System (loadLib)
import Util (lg, tLg, numFromStringE, intFromStringE, gmod, rndstr)
import Pattern (purgeScript, replaceModule, importScript, flagFamily)

-- PUBLIC

-- execute all scripts & script pool
runScripts :: forall eff h. STRef h (SystemST h) -> Number -> EpiS eff h Boolean
runScripts ssRef t = do
  systemST <- lift $ readSTRef ssRef
  res <- traverse (handle ssRef t) (keys systemST.scriptRefPool)
  return $ or res
  where
    handle :: forall eff h. STRef h (SystemST h) -> Number -> String -> EpiS eff h Boolean
    handle ssRef t n = do
      systemST <- lift $ readSTRef ssRef
      case (member n systemST.scriptRefPool) of
        true -> do
          sRef <- loadLib n systemST.scriptRefPool "runScripts"
          scr <- lift $ readSTRef sRef
          fn <- lookupScriptFN scr.fn
          case scr.mid of
            Nothing -> throwError $ "No module when running script: " ++ scr.fn
            Just mid -> fn ssRef n t mid sRef
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
  par <-  loadLib "par" dt "ppath par"
  phs <- (loadLib "phase" dt "ppath phase") >>= numFromStringE
  pathN <- loadLib "path" dt "ppath path"
  mRef <- loadLib mid systemST.moduleRefPool "ppath module"
  m <- lift $ readSTRef mRef

  -- lookup path function
  fn <- case pathN of
    "linear" -> return $ \t -> t
    _ -> throwError $ "Unknown par path : " ++ pathN

  -- execute
  let val = fn ((t - phs) * spd)

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
incStd :: forall eff h. ScriptFn eff h
incStd ssRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef
  let dt = scr.dt

  -- get data
  inc   <- (loadLib "inc" dt "incStd inc") >>= intFromStringE
  subN  <- loadLib "sub" dt "incStd sub"
  dim   <- loadLib "dim" dt "incStd dim"
  mRef  <- loadLib mid systemST.moduleRefPool "incStd module"
  m     <- lift $ readSTRef mRef
  sub   <- loadLib subN m.modules "incStd find sub"

  -- index & next data
  let index = flagFamily systemST.moduleLib $ fromFoldable [(Tuple "family" subN), (Tuple "stdlib" "true")]
  subI <- loadLib sub systemST.moduleLibRefs "incStd modlibref"

  nxtPos <- case (A.elemIndex subI index) of
    Nothing -> return 3
    Just i -> return $ (i + inc) `gmod` (A.length index)

  nxtVal <- case (A.index index nxtPos) of
    Nothing -> throwError $ "your index doesnt exist"
    Just v -> return v

  -- create & import blending script
  createScript ssRef mid "default" "blendModule" $ fromFoldable [(Tuple "subN" subN), (Tuple "sub0" sub), (Tuple "sub1" nxtVal), (Tuple "dim" dim)]

  -- remove self
  purgeScript ssRef self

  return false


-- interpolate between two modules
blendModule :: forall eff h. ScriptFn eff h
blendModule ssRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef
  let dt = scr.dt

  -- get data
  subN <- loadLib "subN" dt "blendSub subN"
  sub0 <- loadLib "sub0" dt "blendSub sub0"
  sub1 <- loadLib "sub1" dt "blendSub sub1"
  dim  <- loadLib "dim" dt "incStd dim"

  -- initialize state
  unless (member "st" dt) do
    let new = fromFoldable [(Tuple "st" "init"), (Tuple "tinit" (show t))]
    let dt' = union new dt
    lift $ modifySTRef sRef (\s -> s {dt = dt'})
    return unit

  scr' <- lift $ readSTRef sRef
  let dt' = scr'.dt

  st    <- loadLib "st" dt' "blendSub st"
  tinit <- (loadLib "tinit" dt' "blendSub tinit") >>= numFromStringE

  mRef  <- loadLib mid systemST.moduleRefPool "incStd module"
  m     <- lift $ readSTRef mRef

  subid    <- loadLib subN m.modules "incStd subid"
  subMRef  <- loadLib subid systemST.moduleRefPool "incStd sub"
  subM     <- lift $ readSTRef subMRef

  -- do stuff
  case st of
    -- initialize state
    "init" -> do
      -- create switch module
      switch <- loadLib "smooth_switch" systemST.moduleLib "blendModule"
      let modules = fromFoldable [(Tuple "m0" sub0), (Tuple "m1" sub1)]
      let sub'    = union (fromFoldable [(Tuple "dim" dim), (Tuple "var" subM.var)]) m.sub
      let switch' = switch {sub = sub', modules = modules, var = subM.var}

      -- replace existing module with switch
      swid      <- replaceModule ssRef mid subN subid (Left switch')
      systemST' <- lift $ readSTRef ssRef
      m'        <- lift $ readSTRef mRef

      -- automate intrp var
      sid <- createScript ssRef swid "default" "ppath" $ fromFoldable [(Tuple "par" "intrp"), (Tuple "path" "linear"), (Tuple "spd" "1.0"), (Tuple "phase" (show tinit))]

      -- update state
      let new = fromFoldable [(Tuple "st" "intrp"), (Tuple "sid" sid), (Tuple "swid" swid)]
      let dt'' = union new dt'
      lift $ modifySTRef sRef (\s -> s {dt = dt''})

      return true

    "intrp" -> do
      case (t - tinit) of
        -- we're done
        x | x >= 1000.0 -> do
          -- replace switch module with m1
          swid     <- loadLib "swid" dt' "blendModule finished swid"
          swmodRef <- loadLib swid systemST.moduleRefPool "blendModule finishd swmod"
          swmod    <- lift $ readSTRef swmodRef
          m1       <- loadLib "m1" swmod.modules "blendModule finishd m1"

          replaceModule ssRef mid subN swid (Right m1)
          systemST' <- lift $ readSTRef ssRef

          -- remove self
          purgeScript ssRef self

          return true

        _ -> do
          return false
    _ -> throwError "you fucked something up"


-- PRIVATE

-- find script fuction given name
lookupScriptFN :: forall eff h. String -> EpiS eff h (ScriptFn eff h)
lookupScriptFN n = case n of
  "null"   -> return nullS
  "ppath"  -> return ppath
  "zpath"  -> return zpath
  "incStd" -> return incStd
  "blendModule" -> return blendModule
  _       -> throwError $ "script function not found: " ++ n


-- create a script dynamically & import it
createScript :: forall eff h. STRef h (SystemST h) -> String -> String -> String -> StrMap String -> EpiS eff h String
createScript ssRef mid parent fn dt = do
  systemST <- lift $ readSTRef ssRef
  scr      <- loadLib parent systemST.scriptLib "create script"

  let scr' = scr {fn = fn, dt = union dt scr.dt}
  importScript ssRef (Left scr') mid
