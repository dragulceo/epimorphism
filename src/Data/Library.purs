module Data.Library where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (ExceptT, throwError)
import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Data.Set (Set, empty) as S
import Data.StrMap (StrMap, empty)
import Data.StrMap.ST (STStrMap, delete, peek, poke)
import Graphics.Canvas (CANVAS)

-- redundant
type Epi eff a = ExceptT String (Eff (canvas :: CANVAS, dom :: DOM | eff)) a
type EpiS eff h a = Epi (st :: ST h | eff) a

data SchemaEntryType = SE_St | SE_N | SE_I | SE_B | SE_S | SE_A_St | SE_A_Cx | SE_M_N | SE_M_St
data SchemaEntry = SchemaEntry SchemaEntryType String
type Schema = Array SchemaEntry

-- MAIN DATA TYPES
newtype FamilyRef    = FamilyRef String
newtype ModuleRef    = ModuleRef String
newtype ComponentRef = ComponentRef String
newtype PatternRef   = PatternRef String
newtype ImageRef     = ImageRef String

newtype Script = Script String
newtype Path = Path String
newtype Include = Include String
newtype CodeBlock = CodeBlock String

data Snapshot = Snapshot DateTime String

type Index = {
    id     :: String
  , parent :: String
  , flags  :: S.Set String
  , props  :: StrMap String
}

indexSchema :: Schema
indexSchema = [
    SchemaEntry SE_St   "id"
  , SchemaEntry SE_St   "flags"
  , SchemaEntry SE_M_St "props"
  , SchemaEntry SE_St   "parent"
]


-- System
data SystemConf = SystemConf Index SystemConfD
type SystemConfD = {
    engineConf :: String
  , uiConf     :: String
  , pattern    :: String --PatternRef
  , seed       :: String
}

systemConfSchema :: Schema
systemConfSchema = [
    SchemaEntry SE_St "engineConf"
  , SchemaEntry SE_St "uiConf"
  , SchemaEntry SE_St "pattern"
  , SchemaEntry SE_St "seed"
]

data EngineConf = EngineConf Index EngineConfD
type EngineConfD = {
    kernelDim            :: Int
  , fract                :: Int
  , numAuxBuffers        :: Int
  , audioAnalysisEnabled :: Boolean
  , audioBufferSize      :: Int
}


data UIConf = UIConf Index UIConfD
type UIConfD = {
    canvasId          :: String
  , consoleId         :: String
  , fpsId             :: String
  , showFps           :: Boolean
  , windowState       :: String
  , uiUpdateFreq      :: Int
  , keyboardSwitchSpd :: Number
  , keySet            :: String
  , uiCompLib         :: String
}

uiConfSchema :: Schema
uiConfSchema = [
    SchemaEntry SE_St "canvasId"
  , SchemaEntry SE_St "consoleId"
  , SchemaEntry SE_St "fpsId"
  , SchemaEntry SE_B  "showFps"
  , SchemaEntry SE_St "windowState"
  , SchemaEntry SE_I  "uiUpdateFreq"
  , SchemaEntry SE_N  "keyboardSwitchSpd"
  , SchemaEntry SE_St "keySet"
  , SchemaEntry SE_St "uiCompLib"
]


data Pattern = Pattern Index PatternD
type PatternD = {
    vert            :: ModuleRef
  , main            :: ModuleRef
  , disp            :: ModuleRef
  , vertC           :: ComponentRef
  , mainC           :: ComponentRef
  , dispC           :: ComponentRef
  , defaultImageLib :: String
  , imageLib        :: String
  -- , 3d shit(everything between Engine & Modules)
}


data Family = Family Index FamilyD
type FamilyD = {
    var          :: String
  , dim          :: Int
  , def_comp_ref :: ComponentRef
}

familySchema :: Schema
familySchema = [
    SchemaEntry SE_St "var"
  , SchemaEntry SE_N "dim"
  , SchemaEntry SE_St "def_mcomp_ref"
]


data Component = Component Index ComponentD
type ComponentD = {
    family_ref  :: FamilyRef
  , def_mod_ref :: ModuleRef
  , children    :: StrMap FamilyRef
  , code        :: CodeBlock
  , includes    :: Array Include
}

componentSchema :: Schema
componentSchema = [
    SchemaEntry SE_St "family_ref"
  , SchemaEntry SE_St "def_mod_ref"
  , SchemaEntry SE_M_St "children"
  , SchemaEntry SE_St "code"
  , SchemaEntry SE_A_St "includes"
]

data Module = Module Index ModuleD
type ModuleD = {
    comp_ref :: ComponentRef
  , scripts  :: Array Script
  , modules  :: StrMap ModuleRef
  , par      :: StrMap Path
  , zn       :: Array Path
  , images   :: Array ImageRef
  , sub      :: StrMap String
}


data Image = Image Index ImageD
type ImageD = {
  path :: String
}


data Section = Section Index SectionD
type SectionD = {
  values :: Array String
}


--- LIBRARY
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
    vert            :: ModuleRef
  , main            :: ModuleRef
  , disp            :: ModuleRef
  , vertC           :: ComponentRef
  , mainC           :: ComponentRef
  , dispC           :: ComponentRef
  , defaultImageLib :: String
  , imageLib        :: String
  -- , 3d shit(everything between Engine & Modules)
} where
  libProj (Library {patternLib}) = patternLib
  idx     (Pattern ix _) = ix
  dat     (Pattern _ dt) = dt
  apI     (Pattern ix dt) mut = Pattern (mut ix) dt
  apD     (Pattern ix dt) mut = Pattern ix (mut dt)

instance dtModule :: DataTable Module {
    comp_ref :: ComponentRef
  , scripts  :: Array Script
  , modules  :: StrMap ModuleRef
  , par      :: StrMap Path
  , zn       :: Array Path
  , images   :: Array ImageRef
  , sub      :: StrMap String
} where
  libProj (Library {moduleLib}) = moduleLib
  idx     (Module ix _) = ix
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


-- LIBRARY CRUD
getLib :: forall a ad eff h. (DataTable a ad) => Library h -> String -> String -> EpiS eff h a
getLib lib name msg = do
  res <- liftEff $ peek (libProj lib) name
  -- dbg $ libProj lib
  -- dbg res
  case res of
    Just x -> pure x
    Nothing -> throwError $ msg <>" # Can't find in library: " <> name
  --fromJustE res (msg <>" # Can't find in library: " <> name)

modLib :: forall a ad eff h. (DataTable a ad) => Library h -> String -> a -> EpiS eff h Unit
modLib lib name new = do
  lift $ poke (libProj lib) name new # void

modLib' :: forall a ad eff h. (DataTable a ad) => Library h -> String -> (a -> a) -> EpiS eff h Unit
modLib' lib name mut = do
  lib' <- mut <$> getLib lib name "modLib' mutator"
  lift $ poke (libProj lib) name lib' # void

delLib :: forall a ad eff h. (DataTable a ad) => Library h -> a -> EpiS eff h Unit
delLib lib obj = do
  lift $ delete (libProj lib :: STStrMap h a) (idx obj).id # void


data LibSearch = LibSearch {flags::S.Set String, exclude::S.Set String, props::StrMap String}
emptySearch :: LibSearch
emptySearch = LibSearch {flags: S.empty, exclude: S.empty, props: empty}

searchLib :: forall a ad eff h. (DataTable a ad) => Library h -> LibSearch -> EpiS eff h (Array a)
searchLib lib search = do
  pure []

-- specific finders
getSystemConf :: forall eff h.  Library h -> String -> EpiS eff h SystemConf
getSystemConf (Library {system: Nothing}) msg = do
  throwError $ msg <> ": System not initialized"
getSystemConf lib@(Library {system: (Just system)}) msg = do
  getLib lib system (msg <> ": Can't find system - " <> system)

getSystemConfD :: forall eff h.  Library h -> String -> EpiS eff h SystemConfD
getSystemConfD lib msg = do
  (SystemConf _ systemConfD) <- getSystemConf lib msg
  pure systemConfD

getUIConf :: forall eff h.  Library h -> String -> EpiS eff h UIConf
getUIConf (Library {system: Nothing}) msg = do
  throwError $ msg <> ": System not initialized"
getUIConf lib@(Library {system: (Just system)}) msg = do
  systemConfD <- getSystemConfD lib "getUIConf"
  let name = (systemConfD.uiConf)
  getLib lib name (msg <> ": Can't find uiConf - " <> name)

getUIConfD :: forall eff h.  Library h -> String -> EpiS eff h UIConfD
getUIConfD lib msg = do
  (UIConf _ uiConfD) <- getUIConf lib msg
  pure uiConfD

getEngineConf :: forall eff h.  Library h -> String -> EpiS eff h EngineConf
getEngineConf (Library {system: Nothing}) msg = do
  throwError $ msg <> ": System not initialized"
getEngineConf lib@(Library {system: (Just system)}) msg = do
  systemConfD <- getSystemConfD lib "getEngineConf"
  let name = (systemConfD.engineConf)
  getLib lib name (msg <> ": Can't find engineConf - " <> name)

getEngineConfD :: forall eff h.  Library h -> String -> EpiS eff h EngineConfD
getEngineConfD lib msg = do
  (EngineConf _ engineConfD) <- getEngineConf lib msg
  pure engineConfD
