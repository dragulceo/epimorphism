module Serialize where

import Prelude
import Data.Types (Epi, Schema, SchemaEntry(..), SchemaEntryType(..))
import Data.Array (null)
import Data.Array (sortBy, foldM) as A
import Data.Complex (inCartesian, Cartesian(Cartesian), Complex)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Set (isEmpty, Set, toUnfoldable)
import Data.StrMap (StrMap)
import Data.StrMap (isEmpty, toUnfoldable) as S
import Data.String (joinWith)
import Data.Tuple (Tuple(Tuple))
import Math (abs)
import Text.Format (format, precision)
import Util (unsafeCast, unsafeGetAttr)

-- serializes an object.  will crash if object doesnt match schema
unsafeSerialize :: forall eff a. Schema -> Maybe String -> a -> Epi eff String
unsafeSerialize schema name obj = do
  dt <- A.foldM (serializeEntry obj) "" (A.sortBy schemaSort schema)
  case name of
    Just n -> do pure $ "--" <> n <> dt
    Nothing -> pure dt


schemaSort :: SchemaEntry -> SchemaEntry -> Ordering
schemaSort (SchemaEntry _ a) (SchemaEntry _ b) = compare a b

serializeEntry :: forall eff a. a -> String -> SchemaEntry -> Epi eff String
serializeEntry obj str (SchemaEntry entryType entryName) = do
  let val = unsafeGetAttr obj entryName

  (Tuple clause isEmpty) <- case entryType of
    SE_St -> do
      pure $ Tuple val (val == "")
    SE_N -> do
      let n = unsafeCast val :: Number
      pure $ Tuple (format (precision 3) n) (n == 0.0)
    SE_I -> do
      let i = unsafeCast val :: Int
      pure $ Tuple (show i) (i == 0)
    SE_B -> do
      let b = unsafeCast val :: Boolean
      pure $ Tuple (show b) false
    SE_S -> do
      let s = unsafeCast val
      pure $ Tuple (serializeSet s) (isEmpty s)
    SE_M_St -> do
      let m = unsafeCast val
      pure $ Tuple (serializeStMap m) (S.isEmpty m)
    SE_M_N -> do
      let m = unsafeCast val
      pure $ Tuple (serializeNMap m) (S.isEmpty m)
    SE_A_St -> do
      let a = unsafeCast val
      pure $ Tuple (serializeStArray a) (null a)
    SE_A_Cx -> do
      let a = unsafeCast val
      pure $ Tuple (serializeCxArray a) (null a)

  pure $ case isEmpty of
    true -> str
    false -> str <> "\n" <> entryName <> " " <> clause


serializeSet :: (Set String) -> String
serializeSet set = "{" <> (joinWith ", " $ toUnfoldable set) <> "}"

serializeStMap :: StrMap String -> String
serializeStMap mp = "{" <> (joinWith ", " $ map (\ (Tuple a b) -> a <> ":" <> b) $ S.toUnfoldable mp) <> "}"

serializeNMap :: StrMap Number -> String
serializeNMap mp = "{" <> (joinWith ", " $ map (\ (Tuple a b) -> a <> ":" <> (format (precision 3) b)) $ S.toUnfoldable mp) <> "}"

serializeStArray :: (Array String) -> String
serializeStArray ary = "[" <> (joinWith ", " ary) <> "]"

serializeCxArray :: (Array Complex) -> String
serializeCxArray ary = "[" <> (joinWith ", " (map showCX ary)) <> "]"

showCX :: Complex -> String
showCX z = case (inCartesian z) of
    (Cartesian x y) -> (format (precision 2) x) <>
                       (if y < 0.0 then "-" else "+") <>
                       (format (precision 2) (abs y)) <> "i"
