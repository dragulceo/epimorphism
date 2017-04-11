module Data.Serialize where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, filter, length, replicate, reverse, uncons, zip, foldM)
import Data.Array (foldM) as A
import Data.Complex (Complex)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set, empty, fromFoldable) as Set
import Data.StrMap (StrMap, empty, fromFoldable, insert, lookup, thawST)
import Data.StrMap (foldM) as S
import Data.StrMap.ST (new)
import Data.String (Replacement(..), joinWith, replace, split, stripSuffix, trim)
import Data.String (Pattern(..)) as S
import Data.String.Regex (match)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Types (Component(Component), EngineConf(EngineConf), Epi, EpiS, Family(..), Image(..), Index, Module(..), Pattern(..), Schema, SchemaEntry(SchemaEntry), SchemaEntryType(SE_A_Cx, SE_A_St, SE_M_N, SE_M_St, SE_S, SE_B, SE_I, SE_N, SE_St), Section(..), SystemConf(SystemConf), UIConf(UIConf), componentSchema, engineConfSchema, familySchema, imageSchema, indexSchema, moduleSchema, patternSchema, systemConfSchema, uiConfSchema, Library(..))
import Util (boolFromStringE, cxFromStringE, dbg, fromJustE, inj, intFromStringE, numFromStringE, tryRegex, zipI)

foreign import unsafeSetDataTableAttr :: forall a b eff. a -> String -> String -> b -> Eff eff a
foreign import unsafeGenericDataTableImpl :: forall a r. Schema -> Schema -> (Index -> (Record r) -> a) -> (Set.Set String) -> a

unsafeGenericDataTable :: forall a r. Schema -> Schema -> (Index -> (Record r) -> a) -> a
unsafeGenericDataTable idxSchema schema construct = unsafeGenericDataTableImpl idxSchema schema construct Set.empty

class Serializable a where
  schema :: a -> Schema
  generic :: a

instance scSerializable :: Serializable SystemConf where
  schema a = systemConfSchema
  generic = unsafeGenericDataTable indexSchema systemConfSchema SystemConf

instance ecSerializable :: Serializable EngineConf where
  schema a = engineConfSchema
  generic = unsafeGenericDataTable indexSchema engineConfSchema EngineConf

instance ucSerializable :: Serializable UIConf where
  schema a = uiConfSchema
  generic = unsafeGenericDataTable indexSchema uiConfSchema UIConf

instance pSerializable :: Serializable Pattern where
  schema a = patternSchema
  generic = unsafeGenericDataTable indexSchema patternSchema Pattern

instance mSerializable :: Serializable Module where
  schema a = moduleSchema
  generic = unsafeGenericDataTable indexSchema moduleSchema Module

instance cSerializable :: Serializable Component where
  schema a = componentSchema
  generic = unsafeGenericDataTable indexSchema componentSchema Component

instance fSerializable :: Serializable Family where
  schema a = familySchema
  generic = unsafeGenericDataTable indexSchema familySchema Family

instance iSerializable :: Serializable Image where
  schema a = imageSchema
  generic = unsafeGenericDataTable indexSchema imageSchema Image

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


-- SLIB PARSERS

data SLibError = SLibError String
type SLib = Either SLibError
data SHandle = SHandle String String

parseHandle :: String -> SLib SHandle
parseHandle group = do
  let lines = split (S.Pattern "\n") group
  {head: sig, tail: body} <- handleUn $ uncons lines
  ssig <- handleS $ stripSuffix (S.Pattern "{{") sig
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
  let groups = filter ((/=) "") $ map trim (split (S.Pattern "}}\n") lib)
  fromFoldable <$> traverse (parseSGroup builder) groups

-- CHUNK PARSERS
buildSection :: SHandle -> SLib (Tuple String Section)
buildSection (SHandle sig body) = do
  let tokens = filter ((/=) "") $ split (S.Pattern " ") sig
  name <- getName tokens
  let idx = {id: name, orig: "", flags: Set.empty, props: empty}
  pure $ Tuple name (Section idx {lib: (map trim $ split (S.Pattern "\n") body)})
  where
    getName [x] = pure x
    getName _ = Left $ SLibError $ "expecting only a name in: " <> sig


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
  let comp = filter ((/=) "") $ split (S.Pattern " ") line
  {head, tail} <- fromJustE (uncons comp) ("Error parsing line " <> (show i) <> " of chunk:\n###" <> chunk)

  pure $ insert head (joinWith " " tail) obj

mapRefById :: forall eff h. (StrMap (StrMap StrObj)) -> String -> (Array StrObj) ->
              EpiS eff h (StrMap (StrMap StrObj))
mapRefById res dataType vals = do
  insert dataType <$> (A.foldM insertObj empty vals) <*> (pure res)
  where
    insertObj :: (StrMap StrObj) -> StrObj -> EpiS eff h (StrMap StrObj)
    insertObj res' obj = do
      i <- fromJustE (lookup "id" obj) "Library object missing id :("
      pure $ insert i obj res'

parseLibData :: forall eff h. String -> EpiS eff h (Library h)
parseLibData allData = do
  -- split data
  (Tuple libData sectionData) <-
    case split (S.Pattern "@@@ Sections") allData of
      [a, b] -> pure (Tuple a b)
      _ -> throwError "Can't separate sections"

  -- sections
  sectionLib <- case (parseSLib buildSection sectionData) of
      (Right res') -> pure res'
      (Left (SLibError s)) -> throwError $ "Error parsing sections: " <> s
  sl <- liftEff $ thawST sectionLib

  let chunks = filter ((/=) "") $ map trim $ split (S.Pattern "###") libData
  let res = empty
  strobjs <- A.foldM parseChunk empty (zipI chunks)
  sc <- lift $ new
  ec <- lift $ new
  uc <- lift $ new
  pl <- lift $ new
  fl <- lift $ new
  cl <- lift $ new
  ml <- lift $ new
  il <- lift $ new
  lib <- pure $ Library {
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

  -- parse chunks
  objs <- S.foldM mapRefById empty strobjs
  S.foldM instantiateChunk lib objs
  where
    instantiate :: forall a. (Serializable a) => (StrMap a) -> String -> StrObj -> EpiS eff h (StrMap a)
    instantiate res name obj = do
      insert name <$> S.foldM fields generic obj <*> (pure res)
    instantiateChunk :: Library h -> String -> (StrMap StrObj) -> EpiS eff h (Library h)
    instantiateChunk lib@(Library val) dataType objs = case dataType of
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
          case entryType of
            SE_St -> do
              liftEff $ unsafeSetDataTableAttr obj accs fieldName fieldVal
            SE_N -> do
              n <- numFromStringE fieldVal
              liftEff $ unsafeSetDataTableAttr obj accs fieldName n
            SE_I -> do
              i <- intFromStringE fieldVal
              liftEff $ unsafeSetDataTableAttr obj accs fieldName i
            SE_B -> do
              b <- boolFromStringE fieldVal
              liftEff $ unsafeSetDataTableAttr obj accs fieldName b
            SE_S -> do
              s <- parseSet fieldVal
              liftEff $ unsafeSetDataTableAttr obj accs fieldName s
            SE_M_St -> do
              m <- parseMp fieldVal
              liftEff $ unsafeSetDataTableAttr obj accs fieldName m
            SE_M_N -> do
              mn <- parseMp fieldVal >>= parseNMp
              liftEff $ unsafeSetDataTableAttr obj accs fieldName mn
            SE_A_St -> do
              l <- parseLst fieldVal
              liftEff $ unsafeSetDataTableAttr obj accs fieldName l
            SE_A_Cx -> do
              cx <- parseLst fieldVal >>= parseCLst
              liftEff $ unsafeSetDataTableAttr obj accs fieldName cx
        _ -> throwError $ inj "Found %0 SchemaEntries for %1" [show $ length all_entries, fieldName]
    schemaSel n (SchemaEntry _ sen) = (n == sen)


serializeLibData :: forall eff h. Library h -> EpiS eff h String
serializeLibData lib = pure ""


--serializeLib :: forall a eff h. (Serializable a) => StStrMap h a -> EpiS eff h String
--serializeLib dta = do
