module Data.Library where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, foldM, head, sortBy)
import Data.List (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Set (Set, subset, isEmpty, intersection, fromFoldable) as S
import Data.StrMap (StrMap, freezeST, fromFoldable, insert, isSubmap, values)
import Data.StrMap.ST (STStrMap, delete, peek, poke)
import Data.Tuple (Tuple)
import Data.Types (Component(Component), ComponentD, ComponentRef, EngineConf(EngineConf), EngineConfD, EpiS, Family(Family), Image(Image), Index, Library(Library), Module(Module), ModuleD, Pattern(Pattern), PatternD, Section(Section), SystemConf(SystemConf), SystemConfD, UIConf(UIConf), UIConfD)
import Unsafe.Coerce (unsafeCoerce)
import Util (fromJustE)

data GenericIdx  = GenericIdx Index GenericIdxD
type GenericIdxD = { }

derive instance genericG :: Generic (GenericIdx)

class Indexable a where
  toG   :: a -> GenericIdx
  fromG :: GenericIdx -> a

gidx :: GenericIdx -> Index
gidx (GenericIdx ix _) = ix

gdat :: GenericIdx -> {}
gdat (GenericIdx _ d) = d

idx :: forall a. Indexable a => a -> Index
idx = gidx <<< toG

dat :: forall a b. Indexable a => a -> b
dat x = unsafeCoerce (gdat $ toG x)

apD :: forall a b. Indexable a => a -> (b -> b) -> a
apD x f = let g = toG x in
  fromG $ GenericIdx (gidx g) (unsafeCoerce $ f $ unsafeCoerce $ gdat g)

apI :: forall a. Indexable a => a -> (Index -> Index) -> a
apI x f = let g = toG x in
  fromG $ GenericIdx (f $ gidx g) (gdat g)


class Indexable a <= DataTable a where
  libProj :: forall h. Library h -> STStrMap h a
  sidx :: forall eff h. Library h -> a -> EpiS eff h Index

instance dtSystemConf :: DataTable SystemConf where
  libProj (Library {systemConfLib}) = systemConfLib
--  libSet  (Library l@{systemConfLib}) a = Library l {systemConfLib = a}
  sidx _ = pure <<< idx

instance ixSystemConf :: Indexable SystemConf where
  toG   = unsafeCoerce
  fromG = unsafeCoerce

instance dtEngineConf :: DataTable EngineConf where
  libProj (Library {engineConfLib}) = engineConfLib
  sidx _ = pure <<< idx

instance ixEngineConf :: Indexable EngineConf where
  toG   = unsafeCoerce
  fromG = unsafeCoerce

instance dtUIConf :: DataTable UIConf where
  libProj (Library {uiConfLib}) = uiConfLib
  sidx _ = pure <<< idx

instance ixUIConf :: Indexable UIConf where
  toG   = unsafeCoerce
  fromG = unsafeCoerce

instance dtPattern :: DataTable Pattern where
  libProj (Library {patternLib}) = patternLib
  sidx _ = pure <<< idx

instance ixPattern :: Indexable Pattern where
  toG   = unsafeCoerce
  fromG = unsafeCoerce

instance dtModule :: DataTable Module where
  libProj (Library {moduleLib}) = moduleLib
  sidx    lib m@(Module ix dt) = do
    fm <- family lib m
    pure $ ix {props = insert "family" (idx fm).id ix.props}

instance ixModule :: Indexable Module where
  toG   = unsafeCoerce
  fromG = unsafeCoerce

instance dtComponent :: DataTable Component where
  libProj (Library {componentLib}) = componentLib
  sidx _ = pure <<< idx

instance ixComponent :: Indexable Component where
  toG   = unsafeCoerce
  fromG = unsafeCoerce

instance dtFamily :: DataTable Family where
  libProj (Library {familyLib}) = familyLib
  sidx _ = pure <<< idx

instance ixFamily :: Indexable Family where
  toG   = unsafeCoerce
  fromG = unsafeCoerce

instance dtImage :: DataTable Image where
  libProj (Library {imageLib}) = imageLib
  sidx _ = pure <<< idx

instance ixIage :: Indexable Image where
  toG   = unsafeCoerce
  fromG = unsafeCoerce

instance dtSection :: DataTable Section where
  libProj (Library {sectionLib}) = sectionLib
  sidx _ = pure <<< idx

instance ixSection :: Indexable Section where
  toG   = unsafeCoerce
  fromG = unsafeCoerce

-- LIBRARY CRUD
getLibM :: forall a eff h. (DataTable a) => Library h -> String -> EpiS eff h (Maybe a)
getLibM lib name = do
  liftEff $ peek (libProj lib) name

getLib :: forall a eff h. (DataTable a) => Library h -> String -> String -> EpiS eff h a
getLib lib name msg = do
  res <- getLibM lib name
  fromJustE res (msg <> " - Can't find in library: " <> name)

setLib :: forall a eff h. (DataTable a) => Library h -> String -> a -> EpiS eff h Unit
setLib lib name new = do
  let new' = apI new _ {id = name} -- make sure index.id is set correctly
  lift $ poke (libProj lib) name new' # void

modLib :: forall a eff h. (DataTable a) => Library h -> a -> (a -> a) -> EpiS eff h Unit
modLib lib obj mut = do
  lift $ poke (libProj lib) (idx obj).id (mut obj) # void

modLibD :: forall a ad eff h. (DataTable a) => Library h -> a -> (ad -> ad) -> EpiS eff h Unit
modLibD lib obj mut = do
  lift $ poke (libProj lib) (idx obj).id (apD obj mut) # void

modLibD' :: forall a ad eff h. (DataTable a) => Library h -> (a -> a) -> String -> String -> (ad -> ad) -> EpiS eff h Unit
modLibD' lib idDT name msg mut = do
  obj <- idDT <$> getLib lib name msg
  lift $ poke (libProj lib) name (apD obj mut) # void

delLib :: forall a eff h. (DataTable a) => Library h -> a -> EpiS eff h Unit
delLib lib obj = do
  lift $ delete (libProj lib :: STStrMap h a) (idx obj).id # void


-- SEARCH
data LibSearch = LibSearch {flags::S.Set String, exclude::S.Set String, props::StrMap String}
buildSearch :: Array String -> Array String -> Array (Tuple String String) -> LibSearch
buildSearch flags exclude props =
  LibSearch {flags: S.fromFoldable flags, exclude: S.fromFoldable exclude,
             props: fromFoldable props}

searchLib :: forall a eff h. (DataTable a) => Library h -> LibSearch -> EpiS eff h (Array a)
searchLib lib search = do
  lib' <- liftEff $ freezeST (libProj lib)
  res <- foldM handle [] (toUnfoldable $ values lib')
  pure $ sortBy (\a b -> compare (idx a).id (idx b).id) res
  where
    handle :: (DataTable a) => Array a -> a -> EpiS eff h (Array a)
    handle res elt = do
      ix <- sidx lib elt
      case (matchSearch search ix) of
        false -> pure res
        true -> pure $ cons elt res

matchSearch :: LibSearch -> Index -> Boolean
matchSearch (LibSearch {flags, exclude, props}) elt =
  (S.subset flags eltFlags) &&
  (S.isEmpty $ S.intersection exclude eltFlags) &&
  (isSubmap props eltProps)
  where
    eltProps = elt.props
    eltFlags = elt.flags


-- GLOBAL FINDERS
getSystemConf :: forall eff h.  Library h -> String -> EpiS eff h SystemConf
getSystemConf (Library {system: Nothing}) msg = do
  throwError $ msg <> ": System not initialized"
getSystemConf lib@(Library {system: (Just system)}) msg = do
  getLib lib system (msg <> ": getSystemConf : ")

getSystemConfD :: forall eff h.  Library h -> String -> EpiS eff h SystemConfD
getSystemConfD lib msg = dat <$> getSystemConf lib msg


getUIConf :: forall eff h.  Library h -> String -> EpiS eff h UIConf
getUIConf (Library {system: Nothing}) msg = do
  throwError $ msg <> ": System not initialized"
getUIConf lib@(Library {system: (Just system)}) msg = do
  systemConfD <- dat <$> getSystemConf lib "getUIConf"
  getLib lib systemConfD.uiConf (msg <> ": getUIConf : ")

getUIConfD :: forall eff h.  Library h -> String -> EpiS eff h UIConfD
getUIConfD lib msg = dat <$> getUIConf lib msg

getEngineConf :: forall eff h.  Library h -> String -> EpiS eff h EngineConf
getEngineConf (Library {system: Nothing}) msg = do
  throwError $ msg <> ": System not initialized"
getEngineConf lib@(Library {system: (Just system)}) msg = do
  systemConfD <- dat <$> getSystemConf lib "getEngineConf"
  getLib lib systemConfD.engineConf (msg <> ": getEngineConf :")

getEngineConfD :: forall eff h.  Library h -> String -> EpiS eff h EngineConfD
getEngineConfD lib msg = dat <$> getEngineConf lib msg


getPattern :: forall eff h.  Library h -> String -> EpiS eff h Pattern
getPattern (Library {system: Nothing}) msg = do
  throwError $ msg <> ": System not initialized"
getPattern lib@(Library {system: (Just system)}) msg = do
  systemConfD <- dat <$> getSystemConf lib "getPattern"
  getLib lib systemConfD.pattern (msg <> ": getPattern :")

getPatternD :: forall eff h.  Library h -> String -> EpiS eff h PatternD
getPatternD lib msg = dat <$> getPattern lib msg


-- CAST
mD :: Module -> ModuleD
mD = dat

idM :: Module -> Module
idM = id

cD :: Component -> ComponentD
cD = dat


-- MODULE
component :: forall eff h. Library h -> Module -> EpiS eff h Component
component lib (Module _ modD) = do
  getLib lib modD.component "get component"

family :: forall eff h. Library h -> Module -> EpiS eff h Family
family lib mod@(Module _ modD) = do
  (Component _ comp) <- component lib mod
  fm <- getLib lib comp.family "get family"
  case (dat fm).var == "*" of
    false -> pure fm
    true -> do -- inherit family from first child
      case (head $ toUnfoldable $ values modD.modules) of
        Nothing -> throwError "Family has type *, but no children"
        Just c -> do
          child <- getLib lib c "family * child"
          family lib child
