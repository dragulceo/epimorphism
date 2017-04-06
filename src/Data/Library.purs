module Data.Library where

import Prelude
import Optic.Lens.Simple
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
type CodeBlock = String

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
newtype SystemConfD = SystemConfD {
    engineConf :: String
  , uiConf     :: String
  , pattern    :: String --PatternRef
  , seed       :: String
}
data SystemConf = SystemConf Index SystemConfD

systemConfSchema :: Schema
systemConfSchema = [
    SchemaEntry SE_St "engineConf"
  , SchemaEntry SE_St "uiConf"
  , SchemaEntry SE_St "pattern"
  , SchemaEntry SE_St "seed"
]


newtype EngineConfD = EngineConfD {
    kernelDim            :: Int
  , fract                :: Int
  , numAuxBuffers        :: Int
  , audioAnalysisEnabled :: Boolean
  , audioBufferSize      :: Int
}
data EngineConf = EngineConf Index EngineConfD


newtype UIConfD = UIConfD {
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
data UIConf = UIConf Index UIConfD

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


newtype PatternD = PatternD{
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
data Pattern = Pattern Index PatternD


newtype FamilyD = FamilyD {
    var          :: String
  , dim          :: Int
  , def_comp_ref :: ComponentRef
}
data Family = Family Index FamilyD

familySchema :: Schema
familySchema = [
    SchemaEntry SE_St "var"
  , SchemaEntry SE_N "dim"
  , SchemaEntry SE_St "def_mcomp_ref"
]


newtype ComponentD = ComponentD {
    family_ref  :: FamilyRef
  , def_mod_ref :: ModuleRef
  , children    :: StrMap FamilyRef
  , code        :: CodeBlock
  , includes    :: Array Include
}
data Component = Component Index ComponentD

componentSchema :: Schema
componentSchema = [
    SchemaEntry SE_St "family_ref"
  , SchemaEntry SE_St "def_mod_ref"
  , SchemaEntry SE_M_St "children"
  , SchemaEntry SE_St "code"
  , SchemaEntry SE_A_St "includes"
]

newtype ModuleD = ModuleD {
    comp_ref :: ComponentRef
  , scripts  :: Array Script
  , modules  :: StrMap ModuleRef
  , par      :: StrMap Path
  , zn       :: Array Path
  , images   :: Array ImageRef
  , sub      :: StrMap String
}
data Module = Module Index ModuleD


newtype ImageD = ImageD {
  path :: String
}
data Image = Image Index ImageD


newtype SectionD = SectionD {
  values :: Array String
}
data Section = Section Index SectionD


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
  index :: a -> Index
  table :: a -> ad

instance dtSystemConf :: DataTable SystemConf SystemConfD where
  libProj (Library {systemConfLib}) = systemConfLib
  index   (SystemConf idx _) = idx
  table   (SystemConf _ dt) = dt

instance dtEngineConf :: DataTable EngineConf EngineConfD where
  libProj (Library {engineConfLib}) = engineConfLib
  index   (EngineConf idx _) = idx
  table   (EngineConf _ dt) = dt

instance dtUIConf :: DataTable UIConf UIConfD where
  libProj (Library {uiConfLib}) = uiConfLib
  index   (UIConf idx _) = idx
  table   (UIConf _ dt) = dt

instance dtPattern :: DataTable Pattern where
  libProj (Library {patternLib}) = patternLib
  index   (Pattern idx _) = idx
  table   (Pattern _ dt) = dt

instance dtModule :: DataTable Module where
  libProj (Library {moduleLib}) = moduleLib
  index   (Module idx _) = idx
  table   (Module _ dt) = dt

instance dtComponent :: DataTable Component ComponentD where
  libProj (Library {componentLib}) = componentLib
  index   (Component idx _) = idx
  table   (Component _ dt) = dt

instance dtFamily :: DataTable Family FamilyD where
  libProj (Library {familyLib}) = familyLib
  index   (Family idx _) = idx
  table   (Family _ dt) = dt

instance dtImage :: DataTable Image where
  libProj (Library {imageLib}) = imageLib
  index   (Image idx _) = idx
  table   (Image _ dt) = dt

instance dtSection :: DataTable Section where
  libProj (Library {sectionLib}) = sectionLib
  index   (Section idx _) = idx
  table   (Section _ dt) = dt


-- general crud

getLib :: forall a ad eff h. (DataTable a ad) => Library h -> String -> String -> EpiS eff h a
getLib lib name msg = do
  -- dbg $ libProj lib
  res <- liftEff $ peek (libProj lib) name
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
  lift $ delete (libProj lib :: STStrMap h a) (index obj).id # void


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
  (SystemConfD systemConfD) <- getSystemConfD lib "getUIConf"
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
  (SystemConfD systemConfD) <- getSystemConfD lib "getEngineConf"
  let name = (systemConfD.engineConf)
  getLib lib name (msg <> ": Can't find engineConf - " <> name)

getEngineConfD :: forall eff h.  Library h -> String -> EpiS eff h EngineConfD
getEngineConfD lib msg = do
  (EngineConf _ engineConfD) <- getEngineConf lib msg
  pure engineConfD
