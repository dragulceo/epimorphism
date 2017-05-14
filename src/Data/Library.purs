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
import Util (fromJustE)

class DataTable a ad | a -> ad where
  libProj :: forall h. Library h -> STStrMap h a
--  libSet  :: forall h. Library h -> a -> Library
  idx :: a -> Index
  dat :: a -> ad
  apI :: a -> (Index -> Index) -> a
  apD :: a -> (ad -> ad) -> a
  sidx :: forall eff h. Library h -> a -> EpiS eff h Index

instance dtSystemConf :: DataTable SystemConf {
    engineConf :: String
  , uiConf     :: String
  , pattern    :: String --PatternRef
  , seed       :: String
  , debug      :: Boolean
} where
  libProj (Library {systemConfLib}) = systemConfLib
--  libSet  (Library l@{systemConfLib}) a = Library l {systemConfLib = a}
  idx     (SystemConf ix _) = ix
  dat     (SystemConf _ dt) = dt
  apI     (SystemConf ix dt) mut = SystemConf (mut ix) dt
  apD     (SystemConf ix dt) mut = SystemConf ix (mut dt)
  sidx _ = pure <<< idx

instance dtEngineConf :: DataTable EngineConf {
    kernelDim            :: Int
  , fract                :: Int
  , numAuxBuffers        :: Int
  , audioAnalysisEnabled :: Boolean
  , audioBufferSize      :: Int
} where
  libProj (Library {engineConfLib}) = engineConfLib
  idx     (EngineConf ix _) = ix
  dat     (EngineConf _ dt) = dt
  apI     (EngineConf ix dt) mut = EngineConf (mut ix) dt
  apD     (EngineConf ix dt) mut = EngineConf ix (mut dt)
  sidx _ = pure <<< idx

instance dtUIConf :: DataTable UIConf {
    canvasId          :: String
  , consoleId         :: String
  , fpsId             :: String
  , showFps           :: Boolean
  , windowState       :: String
  , uiUpdateFreq      :: Int
  , keyboardSwitchSpd :: Number
  , keySet            :: String
  , uiCompLib         :: String
} where
  libProj (Library {uiConfLib}) = uiConfLib
  idx     (UIConf ix _) = ix
  dat     (UIConf _ dt) = dt
  apI     (UIConf ix dt) mut = UIConf (mut ix) dt
  apD     (UIConf ix dt) mut = UIConf ix (mut dt)
  sidx _ = pure <<< idx

instance dtPattern :: DataTable Pattern {
    vert            :: String -- ModuleRef
  , main            :: String -- ModuleRef
  , seed            :: String -- ModuleRef
  , disp            :: String -- ModuleRef
  , defaultImageLib :: String
  , imageLib        :: String
} where
  libProj (Library {patternLib}) = patternLib
  idx     (Pattern ix _) = ix
  dat     (Pattern _ dt) = dt
  apI     (Pattern ix dt) mut = Pattern (mut ix) dt
  apD     (Pattern ix dt) mut = Pattern ix (mut dt)
  sidx _ = pure <<< idx

instance dtModule :: DataTable Module {
    component :: String
  , scripts   :: Array String
  , modules   :: StrMap String
  , par       :: StrMap String
  , zn        :: Array String
  , images    :: Array String
  , sub       :: StrMap String
} where
  libProj (Library {moduleLib}) = moduleLib
  idx     (Module ix dt) = ix
  dat     (Module _ dt) = dt
  apI     (Module ix dt) mut = Module (mut ix) dt
  apD     (Module ix dt) mut = Module ix (mut dt)
  sidx    lib m@(Module ix dt) = do
    fm <- family lib m
    pure $ ix {props = insert "family" (idx fm).id ix.props}

instance dtComponent :: DataTable Component {
    family      :: String -- FamilyRef
  , default_mod :: String -- ModuleRef
  , children    :: StrMap String -- FamilyRef
  , code        :: String --CodeBlock
  , includes    :: Array String -- Include
} where
  libProj (Library {componentLib}) = componentLib
  idx     (Component ix _) = ix
  dat     (Component _ dt) = dt
  apI     (Component ix dt) mut = Component (mut ix) dt
  apD     (Component ix dt) mut = Component ix (mut dt)
  sidx _ = pure <<< idx

instance dtFamily :: DataTable Family {
    var          :: String
  , dim          :: Int
  , default_comp :: ComponentRef
} where
  libProj (Library {familyLib}) = familyLib
  idx     (Family ix _) = ix
  dat     (Family _ dt) = dt
  apI     (Family ix dt) mut = Family (mut ix) dt
  apD     (Family ix dt) mut = Family ix (mut dt)
  sidx _ = pure <<< idx

instance dtImage :: DataTable Image {
  path :: String
} where
  libProj (Library {imageLib}) = imageLib
  idx     (Image ix _) = ix
  dat     (Image _ dt) = dt
  apI     (Image ix dt) mut = Image (mut ix) dt
  apD     (Image ix dt) mut = Image ix (mut dt)
  sidx _ = pure <<< idx

instance dtSection :: DataTable Section {
  lib :: Array String
} where
  libProj (Library {sectionLib}) = sectionLib
  idx     (Section ix _) = ix
  dat     (Section _ dt) = dt
  apI     (Section ix dt) mut = Section (mut ix) dt
  apD     (Section ix dt) mut = Section ix (mut dt)
  sidx _ = pure <<< idx

-- LIBRARY CRUD
getLibM :: forall a ad eff h. (DataTable a ad) => Library h -> String -> EpiS eff h (Maybe a)
getLibM lib name = do
  liftEff $ peek (libProj lib) name

getLib :: forall a ad eff h. (DataTable a ad) => Library h -> String -> String -> EpiS eff h a
getLib lib name msg = do
  res <- getLibM lib name
  fromJustE res (msg <> " - Can't find in library: " <> name)

setLib :: forall a ad eff h. (DataTable a ad) => Library h -> String -> a -> EpiS eff h Unit
setLib lib name new = do
  let new' = apI new _ {id = name} -- make sure index.id is set correctly
  lift $ poke (libProj lib) name new' # void

modLib :: forall a ad eff h. (DataTable a ad) => Library h -> a -> (a -> a) -> EpiS eff h Unit
modLib lib obj mut = do
  lift $ poke (libProj lib) (idx obj).id (mut obj) # void

modLibD :: forall a ad eff h. (DataTable a ad) => Library h -> a -> (ad -> ad) -> EpiS eff h Unit
modLibD lib obj mut = do
  lift $ poke (libProj lib) (idx obj).id (apD obj mut) # void

modLibD' :: forall a ad eff h. (DataTable a ad) => Library h -> (a -> a) -> String -> String -> (ad -> ad) -> EpiS eff h Unit
modLibD' lib idDT name msg mut = do
  obj <- idDT <$> getLib lib name msg
  lift $ poke (libProj lib) name (apD obj mut) # void

delLib :: forall a ad eff h. (DataTable a ad) => Library h -> a -> EpiS eff h Unit
delLib lib obj = do
  lift $ delete (libProj lib :: STStrMap h a) (idx obj).id # void


-- SEARCH
data LibSearch = LibSearch {flags::S.Set String, exclude::S.Set String, props::StrMap String}
buildSearch :: Array String -> Array String -> Array (Tuple String String) -> LibSearch
buildSearch flags exclude props =
  LibSearch {flags: S.fromFoldable flags, exclude: S.fromFoldable exclude,
             props: fromFoldable props}

searchLib :: forall a ad eff h. (DataTable a ad) => Library h -> LibSearch -> EpiS eff h (Array a)
searchLib lib search = do
  lib' <- liftEff $ freezeST (libProj lib)
  res <- foldM handle [] (toUnfoldable $ values lib')
  pure $ sortBy (\a b -> compare (idx a).id (idx b).id) res
  where
    handle :: (DataTable a ad) => Array a -> a -> EpiS eff h (Array a)
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
