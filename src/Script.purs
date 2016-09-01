module Script where

import Prelude
import Config (Pattern, ScriptFn, EpiS, SystemST)
import Control.Monad.Except.Trans (throwError, lift)
import Control.Monad.ST (STRef, readSTRef)
import Data.Foldable (or)
import Data.StrMap (member)
import Data.Traversable (traverse)
import Path (incZn, zpath, ppath, zfix, pfix)
import Switch (randomize, incScript, incScript2, incImage, incMod, finishSwitch, incSub, switchChild, switchSub, switchImage)
import System (mSeq, loadLib)

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
  "incScript2"   -> return incScript2
  "switchChild"  -> return switchChild
  "switchSub"    -> return switchSub
  "switchImage"  -> return switchImage
  "incZn"        -> return incZn
  "finishSwitch" -> return finishSwitch
  "randomize"    -> return randomize
  _              -> throwError $ "script function not found: " ++ n


-- execute all scripts & script pool
runScripts :: forall eff h. STRef h (SystemST h) -> STRef h Pattern -> EpiS eff h Boolean
runScripts ssRef pRef = do
  pattern <- lift $ readSTRef pRef
  r0 <- mSeq ssRef (runScripts' ssRef pRef) pattern.main
  r1 <- mSeq ssRef (runScripts' ssRef pRef) pattern.disp
  r2 <- mSeq ssRef (runScripts' ssRef pRef) pattern.vert

  return $ or (r0 ++ r1 ++ r1)
  where
    runScripts' :: STRef h (SystemST h) -> STRef h Pattern -> String -> EpiS eff h Boolean
    runScripts' ssRef pRef mid = do
      systemST <- lift $ readSTRef ssRef
      mRef <- loadLib mid systemST.moduleRefPool "mid! runScripts"
      m <- lift $ readSTRef mRef

      res <- traverse (runScript ssRef pRef mid) m.scripts
      return $ or res
    runScript :: STRef h (SystemST h) -> STRef h Pattern -> String -> String -> EpiS eff h Boolean
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
