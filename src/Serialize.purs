module Serialize where

import Prelude
import Config (Schema, SchemaEntry(..), SchemaEntryType(..))
import Data.Array (sortBy, foldM) as A
import Data.Complex (Complex)
import Data.Either (Either(..))
import Data.List (fromList)
import Data.Set (toList, Set)
import Data.StrMap (StrMap)
import Data.StrMap (toList) as S
import Data.String (joinWith)
import Data.Tuple (Tuple(Tuple))
import Util (unsafeCast, unsafeGetAttr)

data SerializeError = SerializeError String
type Serialize = Either SerializeError

-- serializes an object.  will crase if object doesnt match schema
unsafeSerialize :: forall a. Schema -> String -> a -> Serialize String
unsafeSerialize schema name obj = do
  dt <- A.foldM (serializeEntry obj) "" (A.sortBy schemaSort schema)
  Right $ "--" ++ name ++ dt

schemaSort :: SchemaEntry -> SchemaEntry -> Ordering
schemaSort (SchemaEntry _ a) (SchemaEntry _ b) = compare a b

serializeEntry :: forall a. a -> String -> SchemaEntry -> Serialize String
serializeEntry obj str (SchemaEntry entryType entryName) = do
  let val = unsafeGetAttr obj entryName

  clause <- case entryType of
    SE_St -> do
      return $ val
    SE_N -> do
      return $ (show (unsafeCast val :: Number))
    SE_I -> do
      return $ (show (unsafeCast val :: Int))
    SE_B -> do
      return $ (show (unsafeCast val :: Boolean))
    SE_S -> do
      return $ serializeSet $ unsafeCast val
    SE_M_St -> do
      return $ serializeStMap $ unsafeCast val
    SE_M_N -> do
      return $ serializeNMap $ unsafeCast val
    SE_A_St -> do
      return $ serializeStArray $ unsafeCast val
    SE_A_Cx -> do
      return $ serializeCxArray $ unsafeCast val

  Right $ str ++ "\n" ++ entryName ++ " " ++ clause


serializeSet :: (Set String) -> String
serializeSet set = "{" ++ (joinWith ", " $ fromList $ toList set) ++ "}"

serializeStMap :: StrMap String -> String
serializeStMap mp = "{" ++ (joinWith ", " $ map (\ (Tuple a b) -> a ++ ":" ++ b) $ fromList $ S.toList mp) ++ "}"

serializeNMap :: StrMap Number -> String
serializeNMap mp = "{" ++ (joinWith ", " $ map (\ (Tuple a b) -> a ++ ":" ++ (show b)) $ fromList $ S.toList mp) ++ "}"

serializeStArray :: (Array String) -> String
serializeStArray ary = "[" ++ (joinWith ", " ary) ++ "]"

serializeCxArray :: (Array Complex) -> String
serializeCxArray ary = "[" ++ (joinWith ", " (map show ary)) ++ "]"
