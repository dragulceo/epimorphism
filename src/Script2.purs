module Script2 where

import Prelude
import Config (EpiS)
import Control.Monad.Except.Trans (throwError)
import Data.Array (foldM, uncons)
import Data.List (fromList)
import Data.Maybe (Maybe(Just))
import Data.StrMap (toList, StrMap, insert, empty)
import Data.String (joinWith, trim, split)
import Data.Tuple (Tuple(Tuple))
import Util (inj, numFromStringE)

data ScriptConfig = ScriptConfig String
data ScriptRes = ScriptRes Boolean Boolean -- compile, update obj
data Script = Script String Number (StrMap String)

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
