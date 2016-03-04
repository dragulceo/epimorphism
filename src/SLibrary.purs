module SLibrary where

import Prelude
import Data.Array (length, filter, uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.String (split, trim, joinWith, stripSuffix)
import Data.StrMap (StrMap (), empty, fromFoldable, foldM, insert, lookup, update)
import Data.Traversable
import Control.Monad (when)

import Config

data SLibError = SLibError String
type SLib = Either SLibError
data SHandle = SHandle String String

parseHandle :: String -> SLib SHandle
parseHandle group = do
  let lines = split "\n" group
  {head: sig, tail: body} <- handleUn $ uncons lines
  ssig <- handleS $ stripSuffix "{{" sig
  return $ SHandle (trim ssig) (joinWith "\n" body)
  where
    handleUn (Just x) = return x
    handleUn _ = Left $ SLibError $ "Your component is too small: " ++ group
    handleS (Just x) = return x
    handleS _ = Left $ SLibError $ "Invalid component format: " ++ group


parseSGroup :: forall a. (SHandle -> SLib (Tuple String a)) -> String -> SLib (Tuple String a)
parseSGroup builder group = do
  handle <- parseHandle group
  builder handle


parseSLib :: forall a. (SHandle -> SLib (Tuple String a)) -> String -> SLib (StrMap a)
parseSLib builder lib = do
  let groups = filter ((/=) "") $ map trim (split "}}\n" lib)
  fromFoldable <$> traverse (parseSGroup builder) groups


-- BUILDERS - maybe move somewhere else?
buildComponent :: SHandle -> SLib (Tuple String Component)
buildComponent (SHandle sig body) = do
  let tokens = filter ((/=) "") $ split " " sig
  name <- getName tokens
  family <- getFamily tokens
  return $ Tuple name {name, family, body}
  where
    getName [_, x] = return x
    getName _ = Left $ SLibError $ "expecting a name in: " ++ sig
    getFamily [x, _] = return x
    getFamily _ = Left $ SLibError $ "expecting a family in: " ++ sig


buildIndex :: SHandle -> SLib (Tuple String Index)
buildIndex (SHandle sig body) = do
  let tokens = filter ((/=) "") $ split " " sig
  name <- getName tokens
  return $ Tuple name {name, lib: (map trim $ split "\n" body)}
  where
    getName [x] = return x
    getName _ = Left $ SLibError $ "expecting only a name in: " ++ sig
