module ScriptUtil where

import Prelude
import Config (EpiS, Pattern, SystemST, Script(Script))
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (modifySTRef, newSTRef, readSTRef, STRef)
import Control.Monad.Trans (lift)
import Data.Array (head, foldM, uncons, deleteAt, cons)
import Data.List (fromList)
import Data.Maybe (fromMaybe, Maybe(Nothing, Just))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (toList, StrMap, insert, empty)
import Data.String (split, joinWith, trim)
import Data.Tuple (Tuple(Tuple))
import Pattern (cloneWith, findModule, findAddr, CloneRes(CloneRes))
import System (mUp)
import Text.Format (precision, format)
import Util (dbg, inj, numFromStringE)

addScript :: forall eff h. SystemST h -> String -> String -> String -> EpiS eff h Unit
addScript systemST mid name args = do
  let scr = inj "%0@%1 %2" [name, (format (precision 2) systemST.t), args]
  mUp systemST mid \m ->
    m {scripts = cons scr m.scripts}

purgeScript :: forall eff h. SystemST h -> String -> Int -> EpiS eff h Unit
purgeScript systemST mid idx = do
  mUp systemST mid \m ->
    m {scripts = fromMaybe m.scripts $ deleteAt idx m.scripts}

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
  inj "%0@%1 %2" [name, (format (precision 2) phase), (serializeArgs args)]
  where
    serializeArgs :: StrMap String -> String
    serializeArgs args = joinWith " " $ fromList $ map (\(Tuple k v) -> k ++ ":" ++ v) (toList args)


-- This method is called by scripts that modify the state tree.  we perform modificatiosn in a cloned tree so we can compile asynchronously
-- I don't like how this modifies ssRef
getClone :: forall eff h. STRef h (SystemST h) -> STRef h Pattern -> String -> EpiS eff h CloneRes
getClone ssRef pRef mid = do
  systemST <- lift $ readSTRef ssRef
  pattern  <- lift $ readSTRef pRef
  case systemST.compPattern of
    Just ref -> do
      dbg "not getting clone"
      pClone <- lift $ readSTRef ref
      addr <- findAddr systemST.moduleRefPool pattern mid
      mid' <- findModule systemST.moduleRefPool pClone addr false
      root <- return $ fromJust $ head $ split "." addr
      return $ CloneRes root pClone mid'
    Nothing -> do
      dbg "cloning pattern"
      cr@(CloneRes _ cpat _) <- cloneWith ssRef pattern mid
      ref <- lift $ newSTRef cpat
      lift $ modifySTRef ssRef (\s -> s {compPattern = Just ref})
      return cr
