module ScriptUtil where

import Prelude
import Config (Script(Script), SystemST, EpiS)
import Control.Monad.Except.Trans (throwError)
import Data.Array (foldM, uncons, deleteAt, cons)
import Data.List (fromList)
import Data.Maybe (fromMaybe, Maybe(Just))
import Data.StrMap (toList, StrMap, insert, empty)
import Data.String (joinWith, trim, split)
import Data.Tuple (Tuple(Tuple))
import System (mUp)
import Util (lg, inj, numFromStringE)

addScript :: forall eff h. SystemST h -> String -> String -> String -> EpiS eff h Unit
addScript systemST mid name args = do
  let scr = inj "%0@%1 %2" [name, (show systemST.t), args]
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
  inj "%0@%1 %2" [name, (show phase), (serializeArgs args)]
  where
    serializeArgs :: StrMap String -> String
    serializeArgs args = joinWith " " $ fromList $ map (\(Tuple k v) -> k ++ ":" ++ v) (toList args)
