module Pattern where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Data.Array (cons, head, tail, foldM, uncons, reverse)
import Data.Library (apD, apI, dat, delLib, family, getLib, getLibM, getPattern, idx, mD, modLibD, setLib)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (insert) as Set
import Data.StrMap (insert, values, toUnfoldable)
import Data.String (Pattern(..)) as S
import Data.String (split, joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Types (EpiS, Module(..), Pattern(..), PatternD, Library)
import System (loadLib)
import Util (fromJustE, log, uuid)


------------------------ FIND ------------------------

-- find a module given an address - ie main.application.t or a reference
findModule :: forall eff h. Library h -> PatternD -> String -> Boolean -> EpiS eff h String
findModule lib patternD mid followSwitch = do
  --lift $ log $ "fm mid " <> mid
  elt <- getLibM lib mid
  case (elt :: Maybe Module) of
    Just _ -> pure mid
    Nothing -> do
      case (uncons $ split (S.Pattern ".") mid) of
        Nothing -> throwError $ "invalid address: " <> mid
        Just {head: addr, tail: rst} -> do
          case addr of
            "vert" -> findModule' lib patternD.vert rst followSwitch
            "disp" -> findModule' lib patternD.disp rst followSwitch
            "seed" -> findModule' lib patternD.seed rst followSwitch
            "main" -> findModule' lib patternD.main rst followSwitch
            x      -> throwError $ "value should be main, vert, or disp : " <> x


findModule' :: forall eff h. Library h -> String -> Array String -> Boolean -> EpiS eff h String
findModule' lib mid addr followSwitch = do
  --lift $ log $ "fm' mid " <> mid
  --lift $ log $ "fm addr " <> (joinWith "." addr)
  maybe (pure $ mid) handle (head addr)
  where
    handle mid' = do
      modD    <- mD <$> getLib lib mid "findModule'"
      childId <- loadLib mid' modD.modules "findModule' find child"
      child@(Module _ childD) <- getLib lib childId "findModule' child"
      addr'   <- fromJustE (tail addr) "shouldn be safe1"

      fm <- family lib child
      case ((idx fm).id == "switch" && followSwitch) of
        true  -> findModule' lib childId (cons "m1" addr') followSwitch
        false -> findModule' lib childId addr' followSwitch


findAddr :: forall eff h. Library h -> PatternD -> String -> EpiS eff h String
findAddr lib patternD mid = do
  m0 <- find' "main" patternD.main
  m1 <- find' "seed" patternD.seed
  m2 <- find' "disp" patternD.disp
  m3 <- find' "vert" patternD.vert

  fromJustE (m0 <> m1 <> m2 <> m3) ("orphan module? " <> mid)
  where
    find' :: String -> String -> EpiS eff h (Maybe String)
    find' addr cid = case (cid == mid) of
      true -> pure (Just addr)
      false -> do
        modD <- mD <$> getLib lib cid "cid findAddr"
        foldM (search addr) Nothing (toUnfoldable modD.modules)
    search addr val (Tuple k v) = do
      res <- find' (addr <> "." <> k) v
      pure $ val <> res


findParent :: forall eff h. Library h -> PatternD -> String -> EpiS eff h (Tuple String String)
findParent lib patternD mid = do
  addr <- findAddr lib patternD mid
  let cmp = split (S.Pattern ".") addr

  case (uncons $ reverse cmp) of
    Nothing -> throwError $ "malformed address" <> addr
    Just {head: lst, tail: addr'} -> do
      pId <- findModule lib patternD (joinWith "." $ reverse addr') false
      pure $ Tuple pId lst

------------------------ IMPORTING ------------------------

-- import the modules of a pattern into the ref pool
data ImportObj = ImportModule Module | ImportRef String
importPattern :: forall eff h. Library h -> EpiS eff h Unit
importPattern lib =  do
  pattern <- getPattern lib "importPattern"
  let patternD = dat pattern

  -- import all modules
  --lift $ log patternD
  main <- importModule lib (ImportRef patternD.main)
  disp <- importModule lib (ImportRef patternD.disp)
  seed <- importModule lib (ImportRef patternD.seed)
  vert <- importModule lib (ImportRef patternD.vert)

  modLibD lib pattern _ {main = main, disp = disp, seed = seed, vert = vert}

  pure unit


-- import a module into the ref pool
importModule :: forall eff h. Library h -> ImportObj -> EpiS eff h String
importModule lib obj = do
  id <- lift $ uuid
  -- find module
  mod@(Module iD modD) <- case obj of
    ImportModule m -> pure m
    ImportRef n -> do
      m@(Module _ modD) <- getLib lib n "importModule"
      --lift $ log $ "importing n: " <> n
      --lift $ log $ "orig = " <> (idx m).orig
      case (idx m).orig of
        "" -> pure $ apI m _ {orig = n}
        _ ->  pure $ apI m _ {orig = (idx m).orig}

  --lift $ log mod

  -- update library
  let idx' = iD { id = id, flags = Set.insert "live" iD.flags }
  setLib lib id (Module idx' modD)

  -- import children
  let d = toUnfoldable modD.modules :: Array (Tuple String String)
  traverse (importChild id) d

  pure id
  where
    importChild :: String -> (Tuple String String) -> EpiS eff h Unit
    importChild mid (Tuple k v) = do
      when (v /= "") do
        -- import child
        child <- importModule lib (ImportRef v)

        -- update parent
        mod@(Module _ modD) <- getLib lib mid "import module - update parent"
        modLibD lib mod _ {modules = insert k child modD.modules}

-- remove a module from the ref pool
purgeModule :: forall eff h. Library h -> String -> EpiS eff h Unit
purgeModule lib mid = do
  when (mid /= "") do

    -- delete self
    mod@(Module _ modD) <- getLib lib mid "purge module"
    delLib lib mod

    -- purge children
    traverse (purgeModule lib) (values modD.modules)

    pure unit


-- replace child subN:cid(in ref pool) with child subN:obj
replaceModule :: forall eff h. Library h -> String -> String -> String -> ImportObj -> EpiS eff h String
replaceModule lib mid subN cid obj = do
  mod@(Module _ modD) <- getLib lib mid "replace module"

  -- import & purge
  n' <- importModule lib obj
  purgeModule lib cid

  -- update
  modLibD lib mod _ {modules = insert subN n' modD.modules}

  pure n'


--  slightly janky
data CloneRes = CloneRes String Pattern String
clonePattern :: forall eff h. Library h -> Pattern -> EpiS eff h Pattern
clonePattern lib pattern@(Pattern _ patternD) = do
  main' <- importModule lib (ImportRef patternD.main)
  disp' <- importModule lib (ImportRef patternD.disp)
  seed' <- importModule lib (ImportRef patternD.seed)
  vert' <- importModule lib (ImportRef patternD.vert)

  pure $ apD pattern _ {main = main', disp = disp', vert = vert', seed = seed'}
