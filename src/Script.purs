module Script where

import Prelude
import Config (PMut(PMutMain, PMutNone), Script(Script), ScriptRes(ScriptRes), Pattern, SystemST, ScriptFn, EpiS)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (readSTRef, STRef)
import Control.Monad.Trans (lift)
import Data.Array (foldM, length, elemIndex, updateAt, (..), zip)
import Data.Foldable (or)
import Data.Maybe (Maybe(Just))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (keys, member)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import ScriptUtil (serializeScript, parseScript)
import Scripts (null, randomize, pause, incZn)
import Switch (finishSwitch, switch)
import System (mFold, mUp, loadLib)
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


-- execute all scripts & script pool. abort as soon as something mutates the pattern
runScripts :: forall eff h. STRef h (SystemST h) -> STRef h Pattern -> EpiS eff h PMut
runScripts ssRef pRef = do
  pattern <- lift $ readSTRef pRef
  r0 <- mFold ssRef PMutNone pattern.main (runModScripts ssRef)
  r1 <- mFold ssRef r0 pattern.disp (runModScripts ssRef)
  mFold ssRef r1 pattern.vert (runModScripts ssRef)


runModScripts :: forall eff h. STRef h (SystemST h) -> PMut -> String -> EpiS eff h PMut
runModScripts ssRef mut mid = do
  systemST <- lift $ readSTRef ssRef
  mRef     <- loadLib mid systemST.moduleRefPool "mid! runScripts"
  m        <- lift $ readSTRef mRef

  foldM (runScript ssRef mid) mut m.scripts


runScript :: forall eff h. STRef h (SystemST h) -> String -> PMut -> String -> EpiS eff h PMut
runScript ssRef mid PMutNone scr = do
  (Script name phase args) <- parseScript scr
  systemST <- lift $ readSTRef ssRef
  fn <- lookupScriptFN name
  let t' = systemST.t - phase

  mRef <- loadLib mid systemST.moduleRefPool "mid! runScript"
  m    <- lift $ readSTRef mRef
  idx  <- return $ fromJust $ elemIndex scr m.scripts
  (ScriptRes mut update) <- fn ssRef t' mid idx args

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

  return mut
runScript ssRef mid x scr = return x
