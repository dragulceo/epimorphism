module Script where

import Prelude
import Config (Pattern, ScriptFn, EpiS, SystemST)
import Control.Monad.Except.Trans (throwError, lift)
import Control.Monad.ST (STRef, readSTRef)
import Data.Foldable (or)
import Data.StrMap (member)
import Data.Traversable (traverse)
import Scripts (incZn, randomize, pause)
import Switch (switch, finishSwitch)
import System (mSeq, loadLib)

-- find script fuction given name
lookupScriptFN :: forall eff h. String -> EpiS eff h (ScriptFn eff h)
lookupScriptFN n = case n of
  "null"         -> return nullS
  "switch"       -> return switch
  "incZn"        -> return incZn
  "finishSwitch" -> return finishSwitch
  "pause"        -> return pause
  "randomize"    -> return randomize
  _              -> throwError $ "script function not found: " ++ n


-- execute all scripts & script pool.  NOTE.  If a script updates the module tree, this isn't reflected until the next time all the scripts are run
runScripts :: forall eff h. STRef h (SystemST h) -> STRef h Pattern -> EpiS eff h Boolean
runScripts ssRef pRef = do
  pattern <- lift $ readSTRef pRef
  r0 <- mSeq ssRef (runModScripts ssRef pRef) pattern.main
  r1 <- mSeq ssRef (runModScripts ssRef pRef) pattern.disp
  r2 <- mSeq ssRef (runModScripts ssRef pRef) pattern.vert

  return $ or (r0 ++ r1 ++ r1)

runModScripts :: forall eff h. STRef h (SystemST h) -> STRef h Pattern -> String -> EpiS eff h Boolean
runModScripts ssRef pRef mid = do
  systemST <- lift $ readSTRef ssRef
  mRef <- loadLib mid systemST.moduleRefPool "mid! runScripts"
  m <- lift $ readSTRef mRef

  res <- traverse (runScript ssRef pRef mid) m.scripts
  return $ or res

runScript :: forall eff h. STRef h (SystemST h) -> STRef h Pattern -> String -> String -> EpiS eff h Boolean
runScript ssRef pRef mid sid = do
  systemST <- lift $ readSTRef ssRef
  case (member sid systemST.scriptRefPool) of
    true -> do
      sRef <- loadLib sid systemST.scriptRefPool "runScripts"
      scr  <- lift $ readSTRef sRef
      fn   <- lookupScriptFN scr.fn
      let t' = systemST.t - scr.tPhase
      fn ssRef pRef sid t' mid sRef
    false -> do -- script purged by previous script
      return false


-- dont do anything
nullS :: forall eff h. ScriptFn eff h
nullS ssRef pRef self t mid sRef = do
  return false
