module Script where

import Prelude
import Config (Script(Script), ScriptRes(ScriptRes), Pattern, SystemST, ScriptFn, EpiS)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (readSTRef, STRef)
import Control.Monad.Trans (lift)
import Data.Array (updateAt, length, (..), zip, foldM, uncons)
import Data.Foldable (or)
import Data.List (fromList)
import Data.Maybe (Maybe(Just))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (toList, StrMap, insert, empty)
import Data.String (joinWith, trim, split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Scripts (randomize, pause, incZn)
import Switch (finishSwitch, switch)
import System (mUp, mSeq, loadLib)
import Util (inj, numFromStringE)

-- find script fuction given name
lookupScriptFN :: forall eff h. String -> EpiS eff h (ScriptFn eff h)
lookupScriptFN n = case n of
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

  res <- traverse (runScript ssRef mid) (zip m.scripts (0..(length m.scripts - 1)))
  return $ or res

runScript :: forall eff h. STRef h (SystemST h) -> String -> (Tuple String Int) -> EpiS eff h Boolean
runScript ssRef mid (Tuple scr idx) = do
  (Script name phase args) <- parseScript scr
  systemST <- lift $ readSTRef ssRef
  fn <- lookupScriptFN name
  let t' = systemST.t - phase

  (ScriptRes recompile update) <- fn ssRef t' mid args
  case update of
    Just dt -> do
      let new = serializeScript (Script name phase dt)
      mUp systemST mid \m ->
        m {scripts = fromJust $ updateAt idx new m.scripts}
    _ -> return unit


  return recompile


-------------------- SERIALIZATION --------------------

parseScript :: forall eff h. String -> EpiS eff h Script
parseScript dta = do
  let dta' = split " " $ trim dta

  --let a = lg dta'
  {head: name, tail: allargs} <- case uncons dta' of
    Just x -> return x
    _ -> throwError "invalid path syntax"

  (Tuple name' phase) <- case (split "@" name) of
    [x] -> return $ Tuple x 0.0
    [x, y] -> do
      y' <- numFromStringE y
      return $ Tuple x y'
    _ -> throwError "wtf are you doing?2"

  args <- foldM parseArg empty allargs

  return $ Script name' phase args
  where
    parseArg dt arg = do
      case (split ":" arg) of
        [k, v] -> return $ insert k v dt
        _ -> throwError $ "malformed arg " ++ arg


serializeScript :: Script -> String
serializeScript (Script name phase args) =
  inj "%0@%1 %2" [name, (show phase), (serializeArgs args)]
  where
    serializeArgs :: StrMap String -> String
    serializeArgs args = joinWith " " $ fromList $ map (\(Tuple k v) -> k ++ ":" ++ v) (toList args)
