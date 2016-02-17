module Library where

import Prelude
import Data.Array (cons, foldM, length, index, drop, filter) as A
import Data.Either (Either(..))
import Data.String (split, joinWith, stripPrefix, trim, contains)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(Nothing, Just), isJust)
import Data.Maybe.Unsafe
import Data.StrMap (StrMap (), empty, fromFoldable, foldM, insert, lookup, update)
import Data.Traversable
import Data.Int (fromString) as I

import Config
import Engine(defaultEngineConf)
import UI(defaultUIConf)
import JSUtil(reallyUnsafeLog)

numFromString :: String -> Maybe Number
numFromString = numFromStringImpl Just Nothing

foreign import numFromStringImpl :: (forall a. a -> Maybe a)
                              -> (forall a. Maybe a)
                              -> String
                              -> Maybe Number

data LibError = LibError String
type Lib = Either LibError

data LineResult = Name String
                | Asgn String String
                | Lst String (Array String)
                | Mp String String String

data LineVal = LAsgn String
             | LLst (Array String)
             | LMp (StrMap String)

fromLAsgn :: String -> LineVal -> Lib String
fromLAsgn _ (LAsgn s) = return s
fromLAsgn s _ = Left (LibError $ "Invalid Type - expected LAsgn in " ++ s)

fromLLst :: String -> LineVal -> Lib (Array String)
fromLLst _ (LLst s) = return s
fromLLst s _ = Left (LibError $ "Invalid Type - expected LLst in " ++ s)

fromLMp :: String -> LineVal -> Lib (StrMap String)
fromLMp _ (LMp s) = return s
fromLMp s _ = Left (LibError $ "Invalid Type - expected LMp in " ++ s)

parseLibLine :: String -> Lib LineResult
parseLibLine line = do
  let tokens = split " " line
  let at0 = A.index tokens 0
  let at1 = A.index tokens 1
  case (A.length tokens) of
    0 -> Left (LibError $ "you broke something: " ++ line)
    1 -> getName line
    otherwise -> do
      let lst = rem 1 tokens
      if (isLst lst) then
        return $ Lst (fromJust at0) (map trim (split "," lst)) else
        case (A.length tokens) of
          2 -> return $ Asgn (fromJust at0) (fromJust at1)
          otherwise -> return $ Mp (fromJust at0) (fromJust at1) (rem 2 tokens)
  where
    getName :: String -> Lib LineResult
    getName line = case (stripPrefix "--" line) of
      (Just n) -> return (Name n)
      Nothing -> Left (LibError $ "expecting a name: " ++ line)
    rem n tokens = joinWith " " $ A.drop n tokens
    isLst token = contains "," token


aggregateLibLines :: Array LineResult -> Lib (Tuple (Maybe String) (StrMap LineVal))
aggregateLibLines lines =
  A.foldM handle (Tuple Nothing empty) lines
  where
    handle (Tuple name dt) (Name n)   = return $ Tuple (Just n) dt
    handle (Tuple name dt) (Asgn n v) = return $ Tuple name (insert n (LAsgn v) dt)
    handle (Tuple name dt) (Lst n v)  = return $ Tuple name (insert n (LLst v) dt)
    handle (Tuple name dt) (Mp n k v) = return $ Tuple name (insert n (LMp (insert k v (def $ lookup n dt))) dt)
    handle _ _ = Left (LibError "AggLibLines: wtf are you doing")
    def (Just (LMp m)) = m
    def _  = empty -- could have errors here


parseGroup :: forall a.  (StrMap LineVal -> Lib a) -> String -> Lib (Tuple String a)
parseGroup builder group = do
  let lines = A.filter ((/=) "") $ split "\n" group
  parsed <- traverse parseLibLine lines
  (Tuple name vals) <- aggregateLibLines parsed
  grp <- builder vals
  res name grp
  where
    res (Just n) grp = return $ Tuple n grp
    res Nothing _ = Left $ LibError $ "name your group chump " ++ group


parseLib :: forall a. (StrMap LineVal -> Lib a) -> String -> Lib (StrMap a)
parseLib builder lib = do
  let groups = split "\n\n" lib
  fromFoldable <$> traverse (parseGroup builder) groups


parseNum :: String -> Lib Number
parseNum s = do
  case (numFromString s) of
    (Just n) -> return n
    _ -> Left $ LibError $ "Expected " ++ s ++ " to be a number"


parseInt :: String -> Lib Int
parseInt s = do
  case (I.fromString s) of
    (Just i) -> return i
    _ -> Left $ LibError $ "Expected " ++ s ++ " to be a int"


-- BUILDERS - maybe move somewhere else?
buildEngineConf :: StrMap LineVal -> Lib EngineConf
buildEngineConf vals = do
  foldM handle defaultEngineConf vals
  where
    handle dt key val = case key of
      "kernelDim" -> (fromLAsgn "kernelDim" val) >>= parseInt >>= (\x -> return $ dt {kernelDim = x})
      "fract" -> (fromLAsgn "fract" val) >>= parseInt >>= (\x -> return $ dt {fract = x})
      otherwise -> Left (LibError $ "EngineConf - unknown key - " ++ key)


buildUIConf :: StrMap LineVal -> Lib UIConf
buildUIConf vals = do
  foldM handle defaultUIConf vals
  where
    handle dt key val = case key of
      "canvasId" -> (fromLAsgn "canvasId" val) >>= (\x -> return $ dt {canvasId = x})
      "consoleId" -> (fromLAsgn "consoleId" val) >>= (\x -> return $ dt {consoleId = x})
      otherwise -> Left (LibError $ "UIConf - unknown key - " ++ key)
