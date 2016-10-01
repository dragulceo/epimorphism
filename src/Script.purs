module Script where

import Prelude
import Config (Script(Script), ScriptRes(ScriptRes), Pattern, SystemST, ScriptFn, EpiS)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (readSTRef, STRef)
import Control.Monad.Trans (lift)
import Data.Array (length, elemIndex, updateAt, (..), zip)
import Data.Foldable (or)
import Data.Maybe (Maybe(Just))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (keys, member)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import ScriptUtil (serializeScript, parseScript)
import Scripts (null, randomize, pause, incZn)
import Switch (finishSwitch, switch)
import System (mUp, mSeq, loadLib)
import Util (lg)

-- find script fuction given name
lookupScriptFN :: forall eff h. String -> EpiS eff h (ScriptFn eff h)
lookupScriptFN n = case n of
  "null"         -> return null
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
  r0 <- mSeq ssRef (runModScripts ssRef) pattern.main
  r1 <- mSeq ssRef (runModScripts ssRef) pattern.disp
  r2 <- mSeq ssRef (runModScripts ssRef) pattern.vert

  return $ or (r0 ++ r1 ++ r1)


runModScripts :: forall eff h. STRef h (SystemST h) -> String -> EpiS eff h Boolean
runModScripts ssRef mid = do
  systemST <- lift $ readSTRef ssRef
  mRef <- loadLib mid systemST.moduleRefPool "mid! runScripts"
  m <- lift $ readSTRef mRef

  res <- traverse (runScript ssRef mid) m.scripts
  return $ or res


runScript :: forall eff h. STRef h (SystemST h) -> String -> String -> EpiS eff h Boolean
runScript ssRef mid scr = do
  (Script name phase args) <- parseScript scr
  systemST <- lift $ readSTRef ssRef
  fn <- lookupScriptFN name
  let t' = systemST.t - phase

  case (member mid systemST.moduleRefPool) of
    true -> do
      mRef <- loadLib mid systemST.moduleRefPool "mid! runScript"
      m    <- lift $ readSTRef mRef
      idx  <- return $ fromJust $ elemIndex scr m.scripts
      (ScriptRes recompile update) <- fn ssRef t' mid idx args

      case update of
        Just dt -> do
          let new = serializeScript (Script name phase dt)
          m'        <- lift $ readSTRef mRef
          systemST' <- lift $ readSTRef ssRef
          idx'      <- return $ fromJust $ elemIndex scr m'.scripts

          mUp systemST' mid \m1 ->
            m1 {scripts = fromJust $ updateAt idx' new m1.scripts}
          return unit
        _ -> return unit

      return recompile
    false -> return false -- purged by another script
