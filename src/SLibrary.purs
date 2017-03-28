module SLibrary where

import Prelude
import Data.Array (filter, uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.String (Pattern(..), split, trim, joinWith, stripSuffix)
import Data.StrMap (StrMap (), fromFoldable)
import Data.Traversable (traverse)

import Config (Index, Component)

data SLibError = SLibError String
type SLib = Either SLibError
data SHandle = SHandle String String

parseHandle :: String -> SLib SHandle
parseHandle group = do
  let lines = split (Pattern "\n") group
  {head: sig, tail: body} <- handleUn $ uncons lines
  ssig <- handleS $ stripSuffix (Pattern "{{") sig
  pure $ SHandle (trim ssig) (joinWith "\n" body)
  where
    handleUn (Just x) = pure x
    handleUn _ = Left $ SLibError $ "Your component is too small: " <> group
    handleS (Just x) = pure x
    handleS _ = Left $ SLibError $ "Invalid component format: " <> group


parseSGroup :: forall a. (SHandle -> SLib (Tuple String a)) -> String -> SLib (Tuple String a)
parseSGroup builder group = do
  handle <- parseHandle group
  builder handle


parseSLib :: forall a. (SHandle -> SLib (Tuple String a)) -> String -> SLib (StrMap a)
parseSLib builder lib = do
  let groups = filter ((/=) "") $ map trim (split (Pattern "}}\n") lib)
  fromFoldable <$> traverse (parseSGroup builder) groups


-- BUILDERS - maybe move somewhere else?
buildComponent :: SHandle -> SLib (Tuple String Component)
buildComponent (SHandle sig body) = do
  let tokens = filter ((/=) "") $ split (Pattern " ") sig
  name <- getName tokens
  family <- getFamily tokens
  pure $ Tuple name {name, family, body}
  where
    getName [_, x] = pure x
    getName _ = Left $ SLibError $ "expecting a name in: " <> sig
    getFamily [x, _] = pure x
    getFamily _ = Left $ SLibError $ "expecting a family in: " <> sig


buildIndex :: SHandle -> SLib (Tuple String Index)
buildIndex (SHandle sig body) = do
  let tokens = filter ((/=) "") $ split (Pattern " ") sig
  name <- getName tokens
  pure $ Tuple name {name, lib: (map trim $ split (Pattern "\n") body)}
  where
    getName [x] = pure x
    getName _ = Left $ SLibError $ "expecting only a name in: " <> sig
