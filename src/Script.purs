module Script where

import Prelude
import Config (Pattern, ScriptFn, EpiS, SystemST)
import Control.Monad.Except.Trans (throwError, lift)
import Control.Monad.ST (STRef, readSTRef)
import Data.Foldable (or)
import Data.StrMap (member, keys)
import Data.Traversable (traverse)
import Path (incZn, zpath, ppath, zfix, pfix)
import Switch (randomize, incScript, incImage, incMod, finishSwitch, incSub)
import System (loadLib)
import Util (lg)

-- find script fuction given name
lookupScriptFN :: forall eff h. String -> EpiS eff h (ScriptFn eff h)
lookupScriptFN n = case n of
  "null"         -> return nullS
  "zfix"         -> return zfix
  "pfix"         -> return pfix
  "ppath"        -> return ppath
  "zpath"        -> return zpath
  "incMod"       -> return incMod
  "incSub"       -> return incSub
  "incImage"     -> return incImage
  "incScript"    -> return incScript
  "incZn"        -> return incZn
  "finishSwitch" -> return finishSwitch
  "randomize"    -> return randomize
  _              -> throwError $ "script function not found: " ++ n


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

          case scr.mid of
            "" -> throwError $ "No module when running script: " ++ scr.fn
            mid -> let t' = systemST.t - scr.tPhase in
              fn ssRef pRef n t' mid sRef
        false -> do
          let g = lg "script removed" -- ghetto(script purged by previous script)
          return false

-- dont do anything
nullS :: forall eff h. ScriptFn eff h
nullS ssRef pRef self t mid sRef = do
  return false
