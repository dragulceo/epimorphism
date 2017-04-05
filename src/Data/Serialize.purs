module Data.Serialize where

import Prelude
import Config (Epi, EpiS, Schema, SchemaEntry(..), SchemaEntryType(..), componentSchema, engineConfSchema, systemConfSchema, uiConfSchema)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, filter, length, uncons)
import Data.Array (foldM) as A
import Data.Library (Library(..), SystemConf, EngineConf, UIConf, Component)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (StrMap, empty, insert, lookup, thawST)
import Data.StrMap (foldM) as S
import Data.StrMap.ST (new)
import Data.String (Pattern(..), Replacement(..), joinWith, replace, split, trim)
import Data.Tuple (Tuple(..))
import Library (parseCLst, parseLst, parseMp, parseNMp, parseSet, unsafeGenericObject)
import Util (boolFromStringE, dbg, dbg2, fromJustE, inj, intFromStringE, numFromStringE, unsafeSetAttr, zipI)

class Serializable a where
  schema :: a -> Schema
  generic :: a

instance scSerializable :: Serializable SystemConf where
  schema a = systemConfSchema
  generic = unsafeGenericObject systemConfSchema

instance ecSerializable :: Serializable EngineConf where
  schema a = engineConfSchema
  generic = unsafeGenericObject engineConfSchema

instance ucSerializable :: Serializable UIConf where
  schema a = uiConfSchema
  generic = unsafeGenericObject uiConfSchema

instance cSerializable :: Serializable Component where
  schema a = componentSchema
  generic = unsafeGenericObject componentSchema


type StrObj = StrMap String

parseChunk :: forall eff. (StrMap (Array StrObj)) -> (Tuple Int String) -> Epi eff (StrMap (Array StrObj))
parseChunk res (Tuple i chunk) = do
  -- extract code block
  (Tuple chunk' code) <- case split (Pattern "&&&\n") chunk of
    [l] -> pure $ Tuple l Nothing
    [l, c] -> pure $ Tuple (l <> "&&&") (Just c)
    _ -> throwError ("Too many code blocks" <> errSuf)

  -- get data type
  let lines = zipI $ split (Pattern "\n") chunk'
  {head: (Tuple _ dataType), tail} <- fromJustE (uncons lines) ("No dataType" <> errSuf)

  -- replace &&& with code in line
  tail' <- case code of
    Nothing -> pure tail
    Just c -> do
      pure $ tail # map (\(Tuple i l) -> (Tuple i (replace (Pattern "&&&") (Replacement c) l)))

  obj <- A.foldM (parseLine chunk') empty tail'

  let arr = maybe [] id (lookup dataType res)
  let arr' = cons obj arr
  pure $ insert dataType arr' res
  where
    errSuf = "\nChunk " <> (show i) <> ":\n" <> "###" <> chunk

parseLine :: forall eff. String -> StrObj -> (Tuple Int String) -> Epi eff StrObj
parseLine chunk obj (Tuple i line) = do
  let comp = filter ((/=) "") $ split (Pattern " ") line
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

--emptyLibrary :: forall h. Library h
--emptyLibrary = do
--  sc <- lift $ new
--  ec <- lift $ new
--  uc <- lift $ new
--  pl <- lift $ new
--  fl <- lift $ new
--  cl <- lift $ new
--  ml <- lift $ new
--  il <- lift $ new
--  sl <- lift $ new
--  Library {
--      systemConfLib: sc
--    , engineConfLib: ec
--    , uiConfLib:     uc
--    , patternLib:    pl
--    , familyLib:     fl
--    , componentLib:  cl
--    , moduleLib:     ml
--    , imageLib:      il
--    , sectionLib:    sl
--    }


parseLibData :: forall eff h. String -> EpiS eff h (Library h)
parseLibData libData = do
  let chunks = filter ((/=) "") $ map trim $ split (Pattern "###") libData
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
  sl <- lift $ new
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
    }

  objs <- S.foldM mapRefById empty strobjs
  S.foldM instantiateChunk lib objs
  where
    instantiate :: forall a. (Serializable a) => (StrMap a) -> String -> StrObj -> EpiS eff h (StrMap a)
    instantiate res name obj = do
      insert name <$> S.foldM fields generic obj <*> (pure res)
    instantiateChunk :: Library h -> String -> (StrMap StrObj) -> EpiS eff h (Library h)
    instantiateChunk lib@(Library val@{}) dataType objs = case dataType of
      "SystemConf" -> do
        obj <- (thawST <$> S.foldM instantiate empty objs) >>= liftEff
        pure $ Library val {systemConfLib = obj}
      "EngineConf" -> do
        obj <- (thawST <$> S.foldM instantiate empty objs) >>= liftEff
        pure $ Library val {engineConfLib = obj}
      "UIConf" -> do
        obj <- (thawST <$> S.foldM instantiate empty objs) >>= liftEff
        pure $ Library val {uiConfLib = obj}
      "Component" -> do
        obj <- (thawST <$> S.foldM instantiate empty objs) >>= liftEff
        pure $ Library val {componentLib = obj}
      _ -> pure lib
    fields :: forall a. (Serializable a) => a -> String -> String -> EpiS eff h a
    fields obj fieldName fieldVal = do
      let ses = filter (schemaSel fieldName) (schema obj)
      case ses of
        [(SchemaEntry entryType _)] -> do
          case entryType of
            SE_St -> do
              pure $ unsafeSetAttr obj fieldName fieldVal
            SE_N -> do
              n <- numFromStringE fieldVal
              pure $ unsafeSetAttr obj fieldName n
            SE_I -> do
              i <- intFromStringE fieldVal
              pure $ unsafeSetAttr obj fieldName i
            SE_B -> do
              b <- boolFromStringE fieldVal
              pure $ unsafeSetAttr obj fieldName b
            SE_S -> do
              s <- parseSet fieldVal
              pure $ unsafeSetAttr obj fieldName s
            SE_M_St -> do
              m <- parseMp fieldVal
              pure $ unsafeSetAttr obj fieldName m
            SE_M_N -> do
              mn <- parseMp fieldVal >>= parseNMp
              pure $ unsafeSetAttr obj fieldName mn
            SE_A_St -> do
              l <- parseLst fieldVal
              pure $ unsafeSetAttr obj fieldName l
            SE_A_Cx -> do
              cx <- parseLst fieldVal >>= parseCLst
              pure $ unsafeSetAttr obj fieldName cx
        _ -> throwError $ inj "Found %0 SchemaEntries for %1" [show $ length ses, fieldName]
    schemaSel n (SchemaEntry _ sen) = (n == sen)


serializeLibData :: forall eff h. Library h -> EpiS eff h String
serializeLibData lib = pure ""
