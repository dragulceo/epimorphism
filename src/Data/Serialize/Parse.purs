module Data.Serialize.Parse where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, filter, length, replicate, reverse, uncons, zip, foldM)
import Data.Array (foldM) as A
import Data.Complex (Complex)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Serializable (class Serializable, generic, schema, unsafeSetDataTableAttr)
import Data.Serialize.SLib (SLibError(..), buildSection, parseSLib)
import Data.Set (Set, empty, fromFoldable) as Set
import Data.StrMap (StrMap, empty, fromFoldable, insert, lookup, thawST)
import Data.StrMap (foldM) as S
import Data.StrMap.ST (STStrMap, new)
import Data.String (Replacement(Replacement), joinWith, replace, split, trim)
import Data.String (Pattern(..)) as S
import Data.String.Regex (match)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Types (Epi, EpiS, Library(Library), SchemaEntry(SchemaEntry), SchemaEntryType(SE_A_Cx, SE_A_St, SE_M_N, SE_M_St, SE_S, SE_B, SE_I, SE_N, SE_St), Section, indexSchema)
import Util (boolFromStringE, cxFromStringE, fromJustE, inj, intFromStringE, numFromStringE, tryRegex, unsafeCast, zipI)

-- ELEMENT PARSERS
parseMString :: forall eff. String -> Epi eff (Maybe String)
parseMString s = do
  pure $ if (s == "Nothing") then Nothing else Just s

parseSet :: forall eff. String -> Epi eff (Set.Set String)
parseSet st = do
  rgx <- tryRegex "^\\{([^\\{\\}]*)\\}$"

  case (match rgx st) of
    (Just [(Just _), (Just "")]) -> do
      pure Set.empty
    (Just [(Just _), (Just l)]) -> pure $ Set.fromFoldable $ map trim $ split (S.Pattern ",") l
    _ -> throwError $ "Expected " <> st <> " to be a set"

parseMp :: forall eff. String -> Epi eff (StrMap String)
parseMp st = do
  rgx <- tryRegex "^\\{([^\\{\\}]*)\\}$"
  case (match rgx st) of
    (Just [(Just _), (Just "")]) -> do
      pure empty
    (Just [(Just _), (Just l)]) -> do
      dt <- traverse (exp <<< split (S.Pattern ":")) $ split (S.Pattern ",") l
      pure $ fromFoldable dt
    _ -> throwError $ "Expected " <> st <> " to be a map1"
  where
    exp [a, b] = pure $ Tuple (trim a) (trim b)
    exp _ = throwError $ "Expected " <> st <> " to be a map2"

parseNMp :: forall eff. StrMap String -> Epi eff (StrMap Number)
parseNMp sm = S.foldM handle empty sm
  where
    handle dt k v = do
      nv <- numFromStringE v
      pure $ insert k nv dt

parseLst :: forall eff. String -> Epi eff (Array String)
parseLst st = do
  rgx <- tryRegex "^\\[([^\\[\\]]*)\\]$"
  case (match rgx st) of
    (Just [(Just _), (Just "")]) -> pure []
    (Just [(Just _), (Just l)]) -> pure $ map trim $ split (S.Pattern ",") l
    _ -> throwError $ "Expected " <> st <> " to be a list"

parseCLst :: forall eff. Array String -> Epi eff (Array Complex)
parseCLst st = reverse <$> foldM handle [] st
  where
    handle dt v = do
      cv <- cxFromStringE v
      pure $ cons cv dt

unsafeParseType :: forall a eff h. SchemaEntryType -> String -> EpiS eff h a
unsafeParseType entryType val = do
  case entryType of
    SE_St ->
      pure $ unsafeCast val
    SE_N -> do
      a <- numFromStringE val
      pure $ unsafeCast a
    SE_I -> do
      a <- intFromStringE val
      pure $ unsafeCast a
    SE_B -> do
      a <- boolFromStringE val
      pure $ unsafeCast a
    SE_S -> do
      a <- parseSet val
      pure $ unsafeCast a
    SE_M_St -> do
      a <- parseMp val
      pure $ unsafeCast a
    SE_M_N -> do
      a <- parseMp val >>= parseNMp
      pure $ unsafeCast a
    SE_A_St -> do
      a <- parseLst val
      pure $ unsafeCast a
    SE_A_Cx -> do
      a <- parseLst val >>= parseCLst
      pure $ unsafeCast a

-- BLOCK PARSERS
type StrObj = StrMap String
parseChunk :: forall eff. (StrMap (Array StrObj)) -> (Tuple Int String) -> Epi eff (StrMap (Array StrObj))
parseChunk res (Tuple i chunk) = do
  -- extract code block
  (Tuple chunk' code) <- case split (S.Pattern "&&&\n") chunk of
    [l] -> pure $ Tuple l Nothing
    [l, c] -> pure $ Tuple (l <> "&&&") (Just c)
    _ -> throwError ("Too many code blocks" <> errSuf)

  -- get data type
  let lines = zipI $ filter ((/=) "") $ map trim $ split (S.Pattern "\n") chunk'
  {head: (Tuple _ dataType), tail} <- fromJustE (uncons lines) ("No dataType" <> errSuf)

  -- replace &&& with code in line
  tail' <- case code of
    Nothing -> pure tail
    Just c -> do
      pure $ tail # map (\(Tuple i l) -> (Tuple i (replace (S.Pattern "&&&") (Replacement c) l)))

  obj <- A.foldM (parseLine chunk') empty tail'

  let arr = maybe [] id (lookup dataType res)
  let arr' = cons obj arr
  pure $ insert dataType arr' res
  where
    errSuf = "\nChunk " <> (show i) <> ":\n" <> "###" <> chunk

parseLine :: forall eff. String -> StrObj -> (Tuple Int String) -> Epi eff StrObj
parseLine chunk obj (Tuple i line) = do
  let comp = split (S.Pattern " ") line
  {head, tail} <- fromJustE (uncons comp) ("Error parsing line " <> (show i) <> " of chunk:\n###" <> chunk)

  let tail' = if head=="code" then tail else (filter ((/=) "") $ tail)

  pure $ insert head (joinWith " " tail') obj


-- LIB PARSERS
mapRefById :: forall eff h. (StrMap (StrMap StrObj)) -> String -> (Array StrObj) ->
              EpiS eff h (StrMap (StrMap StrObj))
mapRefById res dataType vals = do
  insert dataType <$> (A.foldM insertObj empty vals) <*> (pure res)
  where
    insertObj :: (StrMap StrObj) -> StrObj -> EpiS eff h (StrMap StrObj)
    insertObj res' obj = do
      i <- fromJustE (lookup "id" obj) "Library object missing id :("
      pure $ insert i obj res'

emptyLib :: forall eff h.EpiS eff h (Library h)
emptyLib = do
  sc <- lift $ new
  ec <- lift $ new
  uc <- lift $ new
  pl <- lift $ new
  fl <- lift $ new
  cl <- lift $ new
  ml <- lift $ new
  il <- lift $ new
  sl <- lift $ new
  pure $ Library {
      systemConfLib: sc
    , engineConfLib: ec
    , uiConfLib:     uc
    , patternLib:    pl
    , familyLib:     fl
    , componentLib:  cl
    , moduleLib:     ml
    , imageLib:      il
    , sectionLib:    sl
    , system:        Nothing
  }

parseLibData :: forall eff h. String -> EpiS eff h (Library h)
parseLibData allData = do
  -- split data
  (Tuple libData sectionData) <-
    case split (S.Pattern "@@@ Sections") allData of
      [a, b] -> pure (Tuple a b)
      _ -> throwError "Can't separate sections"

  -- parse chunks
  let chunks = filter ((/=) "") $ map trim $ split (S.Pattern "###") libData
  let res = empty
  strobjs <- A.foldM parseChunk empty (zipI chunks)

  -- instantiate chunks
  lib <- emptyLib
  objs <- S.foldM mapRefById empty strobjs
  lib'@(Library ld) <- S.foldM instantiateChunk lib objs

  -- sections
  sectionLib <- case (parseSLib buildSection sectionData) of
    (Right res') -> pure res'
    (Left (SLibError s)) -> throwError $ "Error parsing sections: " <> s
  sl <- liftEff $ thawST sectionLib

  pure $ Library ld {sectionLib = sl}
  where
    instantiate :: forall a. (Serializable a) => (StrMap a) -> String -> StrObj -> EpiS eff h (StrMap a)
    instantiate res name obj = do
      insert name <$> S.foldM fields generic obj <*> (pure res)
    instantiateChunk :: Library h -> String -> (StrMap StrObj) -> EpiS eff h (Library h)
    instantiateChunk lib@(Library val) dataType objs = do
      case dataType of
        "SystemConf" -> do
          obj <- (thawST <$> S.foldM instantiate empty objs) >>= liftEff
          pure $ Library val {systemConfLib = obj}
        "EngineConf" -> do
          obj <- (thawST <$> S.foldM instantiate empty objs) >>= liftEff
          pure $ Library val {engineConfLib = obj}
        "UIConf" -> do
          obj <- (thawST <$> S.foldM instantiate empty objs) >>= liftEff
          pure $ Library val {uiConfLib = obj}
        "Pattern" -> do
          obj <- (thawST <$> S.foldM instantiate empty objs) >>= liftEff
          pure $ Library val {patternLib = obj}
        "Module" -> do
          obj <- (thawST <$> S.foldM instantiate empty objs) >>= liftEff
          pure $ Library val {moduleLib = obj}
        "Component" -> do
          obj <- (thawST <$> S.foldM instantiate empty objs) >>= liftEff
          pure $ Library val {componentLib = obj}
        "Family" -> do
          obj <- (thawST <$> S.foldM instantiate empty objs) >>= liftEff
          pure $ Library val {familyLib = obj}
        "Image" -> do
          obj <- (thawST <$> S.foldM instantiate empty objs) >>= liftEff
          pure $ Library val {imageLib = obj}
        _ -> pure lib
    fields :: forall a. (Serializable a) => a -> String -> String -> EpiS eff h a
    fields obj fieldName fieldVal = do
      let idx_entries  = filter (schemaSel fieldName) indexSchema
      let idx_entries' = zip idx_entries (replicate (length idx_entries) "value0")
      let entries      = filter (schemaSel fieldName) (schema obj)
      let entries'     = zip entries (replicate (length entries) "value1")
      let all_entries = idx_entries' <> entries'
      case all_entries of
        [Tuple (SchemaEntry entryType _) accs] -> do
          val <- unsafeParseType entryType fieldVal
          liftEff $ unsafeSetDataTableAttr obj accs fieldName val
        _ -> throwError $ inj "Found %0 SchemaEntries for %1" [show $ length all_entries, fieldName]
    schemaSel n (SchemaEntry _ sen) = (n == sen)
