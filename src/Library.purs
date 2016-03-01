module Library where

import Prelude
import Data.Array (cons, foldM, length, index, drop, filter, init, reverse) as A
import Data.Complex
import Data.Either (Either(..))
import Data.String (split, joinWith, stripPrefix, trim, contains)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (StrMap (), empty, fromFoldable, foldM, insert, lookup, update)
import Data.Traversable
import Data.Int (fromString) as I

import Config
import Util(lg, numFromString, cxFromString, boolFromString)

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
  let tokens = A.filter ((/=) "") $ split " " line
  let at0 = A.index tokens 0
  let at1 = A.index tokens 1
  case (A.length tokens) of
    0 -> Left (LibError $ "you broke something: " ++ line)
    1 -> getName line
    _ -> do
      let lst = rem 1 tokens
      if (isLst lst) then
        return $ Lst (fromJust at0) (map trim $ fromJust $ A.init (split ".." lst)) else
        case (A.length tokens) of
          2 -> return $ Asgn (fromJust at0) (fromJust at1)
          _ -> return $ Mp (fromJust at0) (fromJust at1) (rem 2 tokens)
  where
    getName :: String -> Lib LineResult
    getName line = case (stripPrefix "--" line) of
      (Just n) -> return (Name n)
      Nothing -> Left (LibError $ "expecting a name: " ++ line)
    rem n tokens = joinWith " " $ A.drop n tokens
    isLst token = contains ".." token


aggregateLibLines :: Array LineResult -> Lib (Tuple (Maybe String) (StrMap LineVal))
aggregateLibLines lines = do
  A.foldM handle (Tuple Nothing empty) lines
  where
    handle (Tuple name dt) (Name n)   = return $ Tuple (Just n) dt
    handle (Tuple name dt) (Asgn n v) = return $ Tuple name (insert n (LAsgn v) dt)
    handle (Tuple name dt) (Lst n v)  = return $ Tuple name (insert n (LLst v) dt)
    handle (Tuple name dt) (Mp n k v) = do
      res <- def n (lookup n dt)
      return $ Tuple name (insert n (LMp (insert k v res)) dt)
    handle _ _ = Left (LibError "AggLibLines: wtf are you doing")
    def :: String -> Maybe LineVal -> Lib (StrMap String)
    def _ (Just (LMp m)) = return m
    def _ Nothing = return empty
    def n _ = Left $ LibError $ "Expected : " ++ n ++ " : to be a map"


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
  let groups = A.filter ((/=) "") $ A.filter ((/=) "\n") $ split "\n\n" lib
  fromFoldable <$> traverse (parseGroup builder) groups

-- PARSERS

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

parseBool :: String -> Lib Boolean
parseBool s = do
  return $ boolFromString s

parseMString :: String -> Lib (Maybe String)
parseMString s = do
  return $ if (s == "Nothing") then Nothing else Just s


parseCX :: String -> Lib Complex
parseCX s = do
  case (cxFromString s) of
    (Just (Tuple r i)) -> return $ outCartesian (Cartesian r i)
    _ -> Left $ LibError $ "Expected " ++ s ++ " to be complex"

parseNMp :: StrMap String -> Lib (StrMap Number)
parseNMp sm = foldM handle empty sm
  where
    handle dt k v = do
      nv <- parseNum v
      return $ insert k nv dt

parseCLst :: Array String -> Lib (Array Complex)
parseCLst sl = A.reverse <$> A.foldM handle [] sl
  where
    handle dt v = do
      cv <- parseCX v
      return $ A.cons cv dt


-- BUILDERS - maybe move somewhere else?
buildSystemConf :: StrMap LineVal -> Lib SystemConf
buildSystemConf vals = do
  foldM handle defaultSystemConf vals
  where
    handle dt key val = case key of
      "initEngineConf" -> (fromLAsgn "initEngineConf" val) >>= (\x -> return $ dt {initEngineConf = x})
      "initUIConf" -> (fromLAsgn "initUIConf" val) >>= (\x -> return $ dt {initUIConf = x})
      "initPattern" -> (fromLAsgn "initPattern" val) >>= (\x -> return $ dt {initPattern = x})
      _ -> Left (LibError $ "SystemConf - unknown key - " ++ key)


buildEngineConf :: StrMap LineVal -> Lib EngineConf
buildEngineConf vals = do
  foldM handle defaultEngineConf vals
  where
    handle dt key val = case key of
      "kernelDim" -> (fromLAsgn "kernelDim" val) >>= parseInt >>= (\x -> return $ dt {kernelDim = x})
      "fract" -> (fromLAsgn "fract" val) >>= parseInt >>= (\x -> return $ dt {fract = x})
      _ -> Left (LibError $ "EngineConf - unknown key - " ++ key)


buildUIConf :: StrMap LineVal -> Lib UIConf
buildUIConf vals = do
  foldM handle defaultUIConf vals
  where
    handle dt key val = case key of
      "canvasId" -> (fromLAsgn "canvasId" val) >>= (\x -> return $ dt {canvasId = x})
      "consoleId" -> (fromLAsgn "consoleId" val) >>= (\x -> return $ dt {consoleId = x})
      "fullScreen" -> (fromLAsgn "fullScreen" val) >>= parseBool >>= (\x -> return $ dt {fullScreen = x})
      _ -> Left (LibError $ "UIConf - unknown key - " ++ key)


defaultModule :: Module
defaultModule = {
    component: ""
  , flags: empty
  , scripts: []
  , modules: empty
  , par: empty
  , zn: []
  , images: []
  , sub: empty
}



buildModule :: StrMap LineVal -> Lib Module
buildModule vals = do
  foldM handle defaultModule vals
  where
    handle dt key val = case key of
      "component" -> (fromLAsgn "component" val) >>= (\x -> return $ dt {component = x})
      "flags" -> (fromLMp "flags" val) >>= (\x -> return $ dt {flags = x})
      "scripts" -> (fromLLst "scripts" val) >>= (\x -> return $ dt {scripts = x})
      "sub" -> (fromLMp "sub" val) >>= (\x -> return $ dt {sub = x})
      "modules" -> (fromLMp "modules" val) >>= (\x -> return $ dt {modules = x})
      "par" -> (fromLMp "par" val) >>= parseNMp >>= (\x -> return $ dt {par = x})
      "zn" -> (fromLLst "zn" val) >>= parseCLst >>= (\x -> return $ dt {zn = x})
      "images" -> (fromLLst "images" val) >>= (\x -> return $ dt {images = x})
      _ -> Left (LibError $ "Module - unknown key - " ++ key)



defaultPattern :: Pattern
defaultPattern = {
    vert: "vert"
  , main: "main"
  , disp: "disp"
  , flags: empty
  , includes: []
  , t: 0.0
  , tPhase: 0.0
  , tSpd: 1.0
}


buildPattern :: StrMap LineVal -> Lib Pattern
buildPattern vals = do
  foldM handle defaultPattern vals
  where
    handle dt key val = case key of
      "vert" -> (fromLAsgn "vert" val) >>= (\x -> return $ dt {vert = x})
      "main" -> (fromLAsgn "main" val) >>= (\x -> return $ dt {main = x})
      "disp" -> (fromLAsgn "disp" val) >>= (\x -> return $ dt {disp = x})
      "flags" -> (fromLMp "flags" val) >>= (\x -> return $ dt {flags = x})
      "includes" -> (fromLLst "includes" val) >>= (\x -> return $ dt {includes = x})
      "t" -> (fromLAsgn "t" val) >>= parseNum >>= (\x -> return $ dt {t = x})
      "tPhase" -> (fromLAsgn "tPhase" val) >>= parseNum >>= (\x -> return $ dt {tPhase = x})
      "tSpd" -> (fromLAsgn "tSpd" val) >>= parseNum >>= (\x -> return $ dt {tSpd = x})
      _ -> Left (LibError $ "Pattern - unknown key - " ++ key)


buildScript :: StrMap LineVal -> Lib Script
buildScript vals = do
  foldM handle defaultScript vals
  where
    handle dt key val = case key of
      "fn" -> (fromLAsgn "fn" val) >>= (\x -> return $ dt {fn = x})
      "mod" -> (fromLAsgn "mod" val) >>= parseMString >>= (\x -> return $ dt {mod = x})
      "flags" -> (fromLMp "flags" val) >>= (\x -> return $ dt {flags = x})
      "dt" -> (fromLMp "dt" val) >>= (\x -> return $ dt {dt = x})
      _ -> Left (LibError $ "Script - unknown key - " ++ key)
