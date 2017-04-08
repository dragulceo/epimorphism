module Data.Library where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (filter, sortBy)
import Data.List (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Set (Set, empty, subset, isEmpty, intersection, fromFoldable) as S
import Data.StrMap (StrMap, empty, freezeST, insert, isSubmap, values, fromFoldable)
import Data.StrMap.ST (STStrMap, delete, peek, poke)
import Data.Tuple (Tuple(..))
import Data.Types (CodeBlock, Component(Component), ComponentRef, EngineConf(EngineConf), EngineConfD, EpiS, Family(Family), FamilyRef, Image(Image), ImageRef, Include, Index, Module(..), ModuleD, ModuleRef, Path, Pattern(Pattern), PatternD, Script, Section(Section), SystemConf(SystemConf), SystemConfD, UIConf(UIConf), UIConfD)
import Util (dbg)

data Library h = Library {
    systemConfLib :: STStrMap h SystemConf
  , engineConfLib :: STStrMap h EngineConf
  , uiConfLib     :: STStrMap h UIConf
  , patternLib    :: STStrMap h Pattern
  , familyLib     :: STStrMap h Family
  , componentLib  :: STStrMap h Component
  , moduleLib     :: STStrMap h Module
  , imageLib      :: STStrMap h Image
  , sectionLib    :: STStrMap h Section
  , system        :: Maybe String
}

class DataTable a ad | a -> ad where
  libProj :: forall h. Library h -> STStrMap h a
  idx :: a -> Index
  dat :: a -> ad
  apI :: a -> (Index -> Index) -> a
  apD :: a -> (ad -> ad) -> a

instance dtSystemConf :: DataTable SystemConf {
    engineConf :: String
  , uiConf     :: String
  , pattern    :: String --PatternRef
  , seed       :: String
} where
  libProj (Library {systemConfLib}) = systemConfLib
  idx     (SystemConf ix _) = ix
  dat     (SystemConf _ dt) = dt
  apI     (SystemConf ix dt) mut = SystemConf (mut ix) dt
  apD     (SystemConf ix dt) mut = SystemConf ix (mut dt)

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

instance dtPattern :: DataTable Pattern {
    vert            :: String -- ModuleRef
  , main            :: String -- ModuleRef
  , disp            :: String -- ModuleRef
  , vertC           :: String -- ComponentRef
  , mainC           :: String -- ComponentRef
  , dispC           :: String -- ComponentRef
  , defaultImageLib :: String
  , imageLib        :: String
  , includes        :: Array String -- REMOVE ME
  -- , 3d shit(everything between Engine & Modules)
} where
  libProj (Library {patternLib}) = patternLib
  idx     (Pattern ix _) = ix
  dat     (Pattern _ dt) = dt
  apI     (Pattern ix dt) mut = Pattern (mut ix) dt
  apD     (Pattern ix dt) mut = Pattern ix (mut dt)

instance dtModule :: DataTable Module {
--    comp_ref :: ComponentRef
--  , scripts  :: Array Script
--  , modules  :: StrMap ModuleRef
--  , par      :: StrMap Path
--  , zn       :: Array Path
--  , images   :: Array ImageRef
--  , sub      :: StrMap String
    component :: String
  , scripts   :: Array String
  , modules   :: StrMap String
  , par       :: StrMap String
  , zn        :: Array String
  , images    :: Array String
  , sub       :: StrMap String
  , var       :: String
  , dim       :: String
  , libName   :: String
  , family    :: String
} where
  libProj (Library {moduleLib}) = moduleLib
  idx     (Module ix dt) = ix {props = insert "family" dt.family ix.props} -- hrm
  dat     (Module _ dt) = dt
  apI     (Module ix dt) mut = Module (mut ix) dt
  apD     (Module ix dt) mut = Module ix (mut dt)

instance dtComponent :: DataTable Component {
    family_ref  :: FamilyRef
  , def_mod_ref :: ModuleRef
  , children    :: StrMap FamilyRef
  , code        :: CodeBlock
  , includes    :: Array Include
} where
  libProj (Library {componentLib}) = componentLib
  idx     (Component ix _) = ix
  dat     (Component _ dt) = dt
  apI     (Component ix dt) mut = Component (mut ix) dt
  apD     (Component ix dt) mut = Component ix (mut dt)

instance dtFamily :: DataTable Family {
    var          :: String
  , dim          :: Int
  , def_comp_ref :: ComponentRef
} where
  libProj (Library {familyLib}) = familyLib
  idx     (Family ix _) = ix
  dat     (Family _ dt) = dt
  apI     (Family ix dt) mut = Family (mut ix) dt
  apD     (Family ix dt) mut = Family ix (mut dt)

instance dtImage :: DataTable Image {
  path :: String
} where
  libProj (Library {imageLib}) = imageLib
  idx     (Image ix _) = ix
  dat     (Image _ dt) = dt
  apI     (Image ix dt) mut = Image (mut ix) dt
  apD     (Image ix dt) mut = Image ix (mut dt)

instance dtSection :: DataTable Section {
  values :: Array String
} where
  libProj (Library {sectionLib}) = sectionLib
  idx     (Section ix _) = ix
  dat     (Section _ dt) = dt
  apI     (Section ix dt) mut = Section (mut ix) dt
  apD     (Section ix dt) mut = Section ix (mut dt)

type Ref a = String
type SCRef = Ref SystemConf
type ECRef = Ref EngineConf
type UCRef = Ref UIConf
type PRef = Ref Pattern
type CRef = Ref Component
type FRef = Ref Family
type MRef = Ref Module
type IRef = Ref Image
type SRef = Ref Section


-- LIBRARY CRUD
getLibM :: forall a ad eff h. (DataTable a ad) => Library h -> String -> EpiS eff h (Maybe a)
getLibM lib name = do
  liftEff $ peek (libProj lib) name

getLib :: forall a ad eff h. (DataTable a ad) => Library h -> String -> String -> EpiS eff h a
getLib lib name msg = do
  res <- getLibM lib name
  case res of
    Just x -> pure x
    Nothing -> throwError $ msg <> " - Can't find in library: " <> name
  --fromJustE res (msg <>" # Can't find in library: " <> name)


setLib :: forall a ad eff h. (DataTable a ad) => Library h -> String -> a -> EpiS eff h Unit
setLib lib name new = do
  lift $ poke (libProj lib) name new # void

modLib :: forall a ad eff h. (DataTable a ad) => Library h -> a -> (a -> a) -> EpiS eff h Unit
modLib lib obj mut = do
  lift $ poke (libProj lib) (idx obj).id (mut obj) # void

modLibD :: forall a ad eff h. (DataTable a ad) => Library h -> a -> (ad -> ad) -> EpiS eff h Unit
modLibD lib obj mut = do
  lift $ poke (libProj lib) (idx obj).id (apD obj mut) # void

delLib :: forall a ad eff h. (DataTable a ad) => Library h -> a -> EpiS eff h Unit
delLib lib obj = do
  lift $ delete (libProj lib :: STStrMap h a) (idx obj).id # void


data LibSearch = LibSearch {flags::S.Set String, exclude::S.Set String, props::StrMap String}
buildSearch :: Array String -> Array String -> Array (Tuple String String) -> LibSearch
buildSearch flags exclude props =
  LibSearch {flags: S.fromFoldable flags, exclude: S.fromFoldable exclude,
             props: fromFoldable props}

searchLib :: forall a ad eff h. (DataTable a ad) => Library h -> LibSearch -> EpiS eff h (Array a)
searchLib lib search = do
  lib' <- liftEff $ freezeST (libProj lib)
  pure $ sortBy (\a b -> compare (idx a).id (idx b).id) $
    filter (matchSearch search) (toUnfoldable $ values lib')

matchSearch :: forall a ad. (DataTable a ad) => LibSearch -> a -> Boolean
matchSearch (LibSearch {flags, exclude, props}) elt =
  (S.subset flags eltFlags) &&
  (S.isEmpty $ S.intersection exclude eltFlags) &&
  (isSubmap props eltProps)
  where
    eltProps = (idx elt).props
    eltFlags = (idx elt).flags


-- specific finders
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
  let name = (systemConfD.uiConf)
  getLib lib name (msg <> ": getUIConf : ")

getUIConfD :: forall eff h.  Library h -> String -> EpiS eff h UIConfD
getUIConfD lib msg = dat <$> getUIConf lib msg

getEngineConf :: forall eff h.  Library h -> String -> EpiS eff h EngineConf
getEngineConf (Library {system: Nothing}) msg = do
  throwError $ msg <> ": System not initialized"
getEngineConf lib@(Library {system: (Just system)}) msg = do
  systemConfD <- dat <$> getSystemConf lib "getEngineConf"
  let name = (systemConfD.engineConf)
  getLib lib name (msg <> ": getEngineConf :")

getEngineConfD :: forall eff h.  Library h -> String -> EpiS eff h EngineConfD
getEngineConfD lib msg = dat <$> getEngineConf lib msg


getPattern :: forall eff h.  Library h -> String -> EpiS eff h Pattern
getPattern (Library {system: Nothing}) msg = do
  throwError $ msg <> ": System not initialized"
getPattern lib@(Library {system: (Just system)}) msg = do
  systemConfD <- dat <$> getSystemConf lib "getPattern"
  let name = (systemConfD.pattern)
  getLib lib name (msg <> ": getPattern :")

getPatternD :: forall eff h.  Library h -> String -> EpiS eff h PatternD
getPatternD lib msg = dat <$> getPattern lib msg


mD :: Module -> ModuleD
mD = dat
