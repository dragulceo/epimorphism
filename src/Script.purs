module Script where

import Prelude
import Config (Pattern, ScriptFn, EpiS, SystemST)
import Control.Monad (unless)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (STRef, modifySTRef, readSTRef)
import Data.Foldable (or)
import Data.StrMap (fromFoldable, insert, member, keys)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Path (zpath, ppath)
import Pattern (findModule')
import ScriptUtil (createScript)
import Switch (incScript, incImage, incMod, finishSwitch, incSub)
import System (loadLib)
import Util (randInt, lg, numFromStringE)

-- PUBLIC

-- execute all scripts & script pool
runScripts :: forall eff h. STRef h (SystemST h) -> STRef h Pattern -> EpiS eff h Boolean
runScripts ssRef pRef = do
  systemST <- lift $ readSTRef ssRef
  res <- traverse (runScript ssRef pRef) (keys systemST.scriptRefPool)
  return $ or res

  where
    runScript :: STRef h (SystemST h) -> STRef h Pattern -> String -> EpiS eff h Boolean
    runScript ssRef pRef n = do
      systemST <- lift $ readSTRef ssRef
      case (member n systemST.scriptRefPool) of
        true -> do
          sRef <- loadLib n systemST.scriptRefPool "runScripts"
          scr  <- lift $ readSTRef sRef
          fn   <- lookupScriptFN scr.fn
          let t' = systemST.t - scr.tPhase
          case scr.mid of
            "" -> throwError $ "No module when running script: " ++ scr.fn
            mid -> fn ssRef pRef n t' mid sRef
        false -> do
          let g = lg "script removed" -- ghetto(script purged by previous script)
          return false

-- SCRIPT FUNCTIONS

-- dont do anything
nullS :: forall eff h. ScriptFn eff h
nullS ssRef pRef self t mid sRef = do
  return false


-- urgh
randomMain :: forall eff h. ScriptFn eff h
randomMain ssRef pRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  pattern  <- lift $ readSTRef pRef
  scr <- lift $ readSTRef sRef

  unless(member "nxt" scr.dt) do
    let dt' = insert "nxt" (show (t + 5.0)) scr.dt
    lift $ modifySTRef sRef (\s -> s {dt = dt'})
    return unit

  scr' <- lift $ readSTRef sRef
  let dt = scr'.dt

  nxt <- (loadLib "nxt" dt "randomMain nxt") >>= numFromStringE

  case t of
    t | t > nxt -> do
      let a = lg "do thing"
      let dt' = insert "nxt" (show (t + 5.0)) dt
      lift $ modifySTRef sRef (\s -> s {dt = dt'})

      idx <- lift $ randInt 100
      tmid <- findModule' systemST.moduleRefPool pattern.main ["main_body", "t"]

      createScript ssRef tmid "default" "incSub" $ fromFoldable [(Tuple "sub" "t_inner"), (Tuple "idx" (show idx)), (Tuple "spd" "0.15"), (Tuple "lib" "t_inner"), (Tuple "dim" "vec2")]

      return false
    _ -> return false


-- PRIVATE

-- find script fuction given name
lookupScriptFN :: forall eff h. String -> EpiS eff h (ScriptFn eff h)
lookupScriptFN n = case n of
  "null"         -> return nullS
  "ppath"        -> return ppath
  "zpath"        -> return zpath
  "incMod"       -> return incMod
  "incSub"       -> return incSub
  "incImage"     -> return incImage
  "incScript"    -> return incScript
  "finishSwitch" -> return finishSwitch
  "randomMain"   -> return randomMain
  _              -> throwError $ "script function not found: " ++ n
