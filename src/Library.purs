module Library where

import Prelude
import Config (Epi, Schema, SchemaEntry(..), SchemaEntryType(..))
import Control.Monad (when)
import Control.Monad.Except.Trans (throwError)
import Data.Array (tail, head, cons, foldM, reverse, filter, length, index) as A
import Data.Complex (outCartesian, Complex, Cartesian(Cartesian))
import Data.Int (fromString) as I
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.Set (Set, fromFoldable, empty) as S
import Data.StrMap (lookup, foldM, StrMap, empty, insert, fromFoldable)
import Data.String (joinWith, null, trim, stripPrefix, split)
import Data.String.Regex (match, noFlags, regex)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Util (numFromString, cxFromString, boolFromString, unsafeSetAttr)

foreign import unsafeGenericObjectImpl :: forall a. Schema -> S.Set String -> a

unsafeGenericObject :: forall a. Schema -> a
unsafeGenericObject schema = unsafeGenericObjectImpl schema S.empty

-- parse an entire library file
parseLib :: forall eff a. Schema -> String -> Epi eff (StrMap a)
parseLib schema lib = do
  let groups = A.filter ((/=) "") $ A.filter ((/=) "\n") $ split "\n\n" lib
  A.foldM (parseGroup schema) empty groups

-- parse an individual element
parseGroup :: forall eff a. Schema -> (StrMap a) -> String -> Epi eff (StrMap a)
parseGroup schema lib group = do
  let lines = A.filter ((/=) "") $ A.filter ((/=) "\n") $ split "\n" group

  -- get name & parent
  {name, parent} <- parseName $ A.head lines
  rst <- return $ fromJust $ A.tail lines -- monadic wrapper, so fromJust is safe

  -- get default obj
  obj <- if (not $ null parent) then
           case (lookup parent lib) of
             (Just p) -> return p
             Nothing  -> throwError $ "couldn't find parent " ++ parent
         else
           return $ unsafeGenericObject schema

  -- set attrs
  final <- A.foldM (parseLine schema) obj rst

  return $ insert name final lib

  where
    parseName (Just line) = do
      case (stripPrefix "--" line) of
        (Just str) -> do
          let cmp = map trim $ A.filter ((/=) "") $ split "<" str
          case cmp of
            [n]    -> return {name: n, parent: ""}
            [n, p] -> return {name: n, parent: p}
            _      -> throwError "wtf are you doing"
        Nothing -> throwError $ "expecting a name: " ++ line
    parseName Nothing = throwError "group doesnt have title line"


-- parse a line & update an object
parseLine :: forall eff a. Schema -> a -> String -> Epi eff a
parseLine schema obj line = do
  let tokens = A.filter ((/=) "") $ split " " line
  case (A.length tokens) of
    0 -> throwError $ "malformed line " ++ line
    1 -> return obj
    _ -> do
      let attrn = fromJust $ A.index tokens 0
      let val = joinWith " " $ fromJust $ A.tail tokens

      let ses = A.filter (schemaSel attrn) schema
      when (A.length ses /= 1) do
        throwError $ line ++ " matched " ++ (show $ A.length ses) ++ " schema entries!!"

      (SchemaEntry st _) <- return $ fromJust $ A.head ses

      case st of
        SE_St -> do
          return $ unsafeSetAttr obj attrn val
        SE_N -> do
          n <- parseNum val
          return $ unsafeSetAttr obj attrn n
        SE_I -> do
          i <- parseInt val
          return $ unsafeSetAttr obj attrn i
        SE_B -> do
          b <- parseBool val
          return $ unsafeSetAttr obj attrn b
        SE_S -> do
          s <- parseSet val
          return $ unsafeSetAttr obj attrn s
        SE_M_St -> do
          m <- parseMp val
          return $ unsafeSetAttr obj attrn m
        SE_M_N -> do
          mn <- parseMp val >>= parseNMp
          return $ unsafeSetAttr obj attrn mn
        SE_A_St -> do
          l <- parseLst val
          return $ unsafeSetAttr obj attrn l
        SE_A_Cx -> do
          cx <- parseLst val >>= parseCLst
          return $ unsafeSetAttr obj attrn cx

  where
    schemaSel n (SchemaEntry _ sen) = (n == sen)


-- ELEMENT PARSERS
parseNum :: forall eff. String -> Epi eff Number
parseNum s = do
  case (numFromString s) of
    (Just n) -> return n
    _ -> throwError $ "Expected " ++ s ++ " to be a number"

parseInt :: forall eff. String -> Epi eff Int
parseInt s = do
  case (I.fromString s) of
    (Just i) -> return i
    _ -> throwError $ "Expected " ++ s ++ " to be a int"

parseBool :: forall eff. String -> Epi eff Boolean
parseBool s = do
  return $ boolFromString s

parseMString :: forall eff. String -> Epi eff (Maybe String)
parseMString s = do
  return $ if (s == "Nothing") then Nothing else Just s

parseCX :: forall eff. String -> Epi eff Complex
parseCX s = do
  case (cxFromString s) of
    (Just (Tuple r i)) -> return $ outCartesian (Cartesian r i)
    _ -> throwError $ "Expected " ++ s ++ " to be complex"

parseSet :: forall eff. String -> Epi eff (S.Set String)
parseSet st = do
  let rgx = regex "^\\{([^\\{\\}]*)\\}$" noFlags
  case (match rgx st) of
    (Just [(Just _), (Just "")]) -> do
      return S.empty
    (Just [(Just _), (Just l)]) -> return $ S.fromFoldable $ map trim $ split "," l
    _ -> throwError $ "Expected " ++ st ++ " to be a set"

parseMp :: forall eff. String -> Epi eff (StrMap String)
parseMp st = do
  let rgx = regex "^\\{([^\\{\\}]*)\\}$" noFlags
  case (match rgx st) of
    (Just [(Just _), (Just "")]) -> do
      return empty
    (Just [(Just _), (Just l)]) -> do
      dt <- traverse (exp <<< split ":") $ split "," l
      return $ fromFoldable dt
    _ -> throwError $ "Expected " ++ st ++ " to be a map1"
  where
    exp [a, b] = return $ Tuple (trim a) (trim b)
    exp _ = throwError $ "Expected " ++ st ++ " to be a map2"

parseNMp :: forall eff. StrMap String -> Epi eff (StrMap Number)
parseNMp sm = foldM handle empty sm
  where
    handle dt k v = do
      nv <- parseNum v
      return $ insert k nv dt

parseLst :: forall eff. String -> Epi eff (Array String)
parseLst st = do
  let rgx = regex "^\\[([^\\[\\]]*)\\]$" noFlags
  case (match rgx st) of
    (Just [(Just _), (Just "")]) -> return []
    (Just [(Just _), (Just l)]) -> return $ map trim $ split "," l
    _ -> throwError $ "Expected " ++ st ++ " to be a list"

parseCLst :: forall eff. Array String -> Epi eff (Array Complex)
parseCLst st = A.reverse <$> A.foldM handle [] st
  where
    handle dt v = do
      cv <- parseCX v
      return $ A.cons cv dt
