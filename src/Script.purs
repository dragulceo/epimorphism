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
import Pattern (purgeScript, replaceModule, importScript)

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
--  let g = lg (t - phs)
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
incIdx :: forall eff h. ScriptFn eff h
incIdx ssRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef
  let dt = scr.dt

  -- get data
  inc  <- (loadLib "inc" dt "incIdx inc") >>= intFromStringE
  subN  <- loadLib "sub" dt "incIdx sub"
  mRef  <- loadLib mid systemST.moduleRefPool "incIdx module"
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

  -- create & import blending script
  createScript ssRef mid "default" "blendSub" $ fromFoldable [(Tuple "subN" subN), (Tuple "sub0" sub), (Tuple "sub1" nxtVal)]

  -- remove self
  purgeScript ssRef self

  return false


-- interpolate between two substitutions
blendSub :: forall eff h. ScriptFn eff h
blendSub ssRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef
  let dt = scr.dt

  -- get data
  subN <- loadLib "subN" dt "blendSub subN"
  sub0 <- loadLib "sub0" dt "blendSub sub0"
  sub1 <- loadLib "sub1" dt "blendSub sub1"

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

  mRef  <- loadLib mid systemST.moduleRefPool "incIdx module"
  m <- lift $ readSTRef mRef
  let sub = m.sub
  let par = m.par

  -- do stuff
  case st of
    "init" -> do
      let g = lg "initializing intrp"
      pid <- lift $ rndstr
      let intrp = "(1.0 - @" ++ pid ++ "@ / 1000.0) * " ++ sub0 ++ " + @" ++ pid ++ "@ / 1000.0 * " ++ sub1  -- man is this messy
      let sub' = insert subN intrp sub
      lift $ modifySTRef mRef (\m -> m {sub = sub'})

      -- add & automate intrp var
      let par' = insert pid 0.0 par
      lift $ modifySTRef mRef (\m -> m {par = par'})

      sid <- createScript ssRef mid "default" "ppath" $ fromFoldable [(Tuple "par" pid), (Tuple "path" "linear"), (Tuple "spd" "1.0"), (Tuple "phase" (show tinit))]

      -- update script
      let new = fromFoldable [(Tuple "st" "intrp"), (Tuple "intrpid" sid), (Tuple "intrppar" pid)]
      let dt'' = union new dt'
      lift $ modifySTRef sRef (\s -> s {dt = dt''})

      return true

    "intrp" -> do
      case (t - tinit) of
        x | x >= 1000.0 -> do
          let g = lg "done intrp"

          -- remove intrp script
          sid <- loadLib "intrpid" dt' "blendSub finished intrpid"
          purgeScript ssRef sid

          -- remove intrp var & replace with sub1
          pid <- loadLib "intrppar" dt' "blendSub finished intrppar"
          let sub' = insert subN sub1 sub
          let par' = delete pid par
          lift $ modifySTRef mRef (\m -> m {par = par', sub = sub'})

          -- remove self
          purgeScript ssRef self
          return true
        _ -> do
          return false
    _ -> throwError "you fucked something up"



-- interpolate between two modules
blendModule :: forall eff h. ScriptFn eff h
blendModule ssRef self t mid sRef = do

    -- do thing
--  let sub' = insert subN nxtVal m.sub
  --lift $ modifySTRef mRef (\m -> m {sub = sub'})

  return false


-- PRIVATE

-- find script fuction given name
lookupScriptFN :: forall eff h. String -> EpiS eff h (ScriptFn eff h)
lookupScriptFN n = case n of
  "null"  -> return nullS
  "ppath" -> return ppath
  "zpath" -> return zpath
  "incIdx" -> return incIdx
  "blendSub" -> return blendSub
  "blendModule" -> return blendModule
  _       -> throwError $ "script function not found: " ++ n


-- create a script dynamically
createScript :: forall eff h. STRef h (SystemST h) -> String -> String -> String -> StrMap String -> EpiS eff h String
createScript ssRef mid parent fn dt = do
  systemST <- lift $ readSTRef ssRef
  scr <- loadLib parent systemST.scriptLib "create script"

  let scr' = scr {fn = fn, dt = union dt scr.dt}
  importScript ssRef (Left scr') mid
