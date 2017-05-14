module Data.Serialize.SLib where

import Prelude
import Data.Array (filter, uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Set (empty) as S
import Data.StrMap (StrMap, fromFoldable, empty)
import Data.String (Pattern(..), joinWith, split, stripSuffix, trim)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Types (Section(Section))


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

-- CHUNK PARSERS
buildSection :: SHandle -> SLib (Tuple String Section)
buildSection (SHandle sig body) = do
  let tokens = filter ((/=) "") $ split (Pattern " ") sig
  name <- getName tokens
  let idx = {id: name, orig: "", flags: S.empty, props: empty}
  pure $ Tuple name (Section idx {lib: (map trim $ split (Pattern "\n") body)})
  where
    getName [x] = pure x
    getName _ = Left $ SLibError $ "expecting only a name in: " <> sig
