module ScriptUtil where

import Prelude
import Control.Monad.Except.Trans (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, delete, foldM, head, uncons)
import Data.Library (dat, getLib, getLibM, idM, modLibD, setLib)
import Data.Maybe (Maybe(Nothing, Just))
import Data.StrMap (StrMap, insert, empty, toUnfoldable)
import Data.String (joinWith, split, trim)
import Data.String (Pattern(..)) as S
import Data.Tuple (Tuple(Tuple))
import Data.Script (Script(Script))
import Data.Types (EpiS, Pattern, Library)
import Pattern (importPattern, findModule, findAddr)
import Text.Format (precision, format)
import Util (log, inj, numFromStringE, fromJustE)

addScript :: forall eff h. Library h -> Number -> String -> String -> String -> EpiS eff h Unit
addScript lib sysT mid name args = do
  let scr = inj "%0@%1 %2" [name, (format (precision 2) sysT), args]
  lift $ log $ "ADD SCRIPT/" <> mid <> ": " <> scr
  mod <- idM <$> getLib lib mid "addScript"
  modLibD lib mod \m ->
    m {scripts = cons scr m.scripts}

purgeScript :: forall eff h. Library h -> String -> String -> EpiS eff h Unit
purgeScript lib mid scr = do
  mod <- idM <$> getLib lib mid "purgeScript"
  lift $ log $ inj "PURGING SCRIPT %0 from [%1] in %2" [scr, (joinWith ", " (dat mod).scripts), mid]
  modLibD lib mod \m ->
    m {scripts = delete scr m.scripts}

parseScript :: forall eff h. String -> EpiS eff h Script
parseScript dta = do
  let dta' = split (S.Pattern " ") $ trim dta

  --let a = lg dta'
  {head: name, tail: allargs} <- case uncons dta' of
    Just x -> pure x
    _ -> throwError "invalid path syntax"

  (Tuple name' phase) <- case (split (S.Pattern "@") name) of
    [x] -> pure $ Tuple x 0.0
    [x, y] -> do
      y' <- numFromStringE y
      pure $ Tuple x y'
    _ -> throwError "wtf are you doing?2"

  args <- foldM parseArg empty allargs

  pure $ Script name' phase args
  where
    parseArg dt arg = do
      case (split (S.Pattern ":") arg) of
        [k, v] -> pure $ insert k v dt
        _ -> throwError $ "malformed arg " <> arg


serializeScript :: Script -> String
serializeScript (Script name phase args) =
  inj "%0@%1 %2" [name, (format (precision 2) phase), (serializeArgs args)]
  where
    serializeArgs :: StrMap String -> String
    serializeArgs args' = joinWith " " $ map (\(Tuple k v) -> k <> ":" <> v) (toUnfoldable args')


-- This method is called by scripts that modify the state tree.  we perform modifications in a cloned tree so we can compile asynchronously
-- I don't like how this modifies ssRef
data CloneRes = CloneRes String Pattern String
getClone :: forall eff h. Library h -> Pattern -> String -> EpiS eff h CloneRes
getClone lib pattern mid = do
  compP <- getLibM lib "$$Comp"
  pattern' <- case (compP :: Maybe Pattern) of
    Just pd -> pure pd
    Nothing -> do
      lift $ log "CLONING PATTERN"
      pattern' <- importPattern lib pattern -- cloning current pattern
      setLib lib "$$Comp" pattern'
      lift $ log lib
      pure pattern'

  addr <- findAddr lib (dat pattern) mid
  mid' <- findModule lib (dat pattern') addr false
  root <- fromJustE (head $ split (S.Pattern ".") addr) "getClone invalid addr"
  pure $ CloneRes root pattern' mid'
