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
import Data.Newtype (class Newtype, unwrap, wrap)
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

derive instance newtypeSystemConfD :: Newtype SystemConfD _
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

class (Newtype ad k) <= DataTable a ad k | a -> ad, ad -> k where
  libProj :: forall h. Library h -> STStrMap h a
  idx :: a -> Index
  dat :: a -> ad
  apI :: a -> (Index -> Index) -> a
  apD :: a -> (k -> k) -> a

--class358 DataTable2 a ad | a -> ad where
  --apDr :: a -> ad -> a
--  apD :: a -> (ad -> ad) -> a

--dmod :: forall a. SystemConf -> String -> a -> SystemConf
--dmod (SystemConf ix (SystemConfD vals)) str val = (SystemConf ix (SystemConfD vals {str=val}))

--newSystemConf :: Index -> SystemConfDR -> SystemConf
--newSystemConf ix dt = SystemConf ix (SystemConfD dt {seed = "asdf"})
--
--newSystemConf (idx sc) (
--
--import Prelude
--newtype Test = Test {a::Int, b::Int}
--instance showTest :: Show Test where show (Test {a, b}) = "Test " <> (show a) <> " " <> (show b)
--instance showP :: Show {a::Int, b::Int} where show {a, b} = (show a) <> " " <> (show b)

sc :: SystemConf
sc = (SystemConf {id: "hi", parent: "", flags: S.empty, props: empty} (SystemConfD {engineConf: "ec", uiConf: "uc", pattern: "p", seed: ""}))

--trans obj f = wrap $ f $ unwrap obj

--sc' = apD sc (\x -> (dat sc))
--sc' = apI sc _ {id = "blorp"}
sc' :: SystemConf
sc' = apD sc _ {seed = "blorp"}
--sc' = apD sc $ \scd -> trans scd _ {seed = "blorp"}



                       --(\(SystemConfD x) -> (SystemConfD (x {seed="blorp"})))

type T = {engineConf :: String, uiConf :: String, pattern :: String, seed :: String}
instance dtSystemConf :: DataTable SystemConf SystemConfD {engineConf :: String, uiConf :: String, pattern :: String, seed :: String} where
  libProj (Library {systemConfLib}) = systemConfLib
  idx     (SystemConf ix _) = ix
  dat     (SystemConf _ dt) = dt
  apI     (SystemConf ix dt) mut = SystemConf (mut ix) dt
  apD     (SystemConf ix dt) mut = SystemConf ix (wrap $ mut $ unwrap dt)

--instance dt2SystemConf :: DataTable2 SystemConf SystemConfD where
--  apD     (SystemConf ix (SystemConfD dt)) mut = SystemConf ix (SystemConfD (mut dt))
--  apDr    (SystemConf ix dt) dt' = SystemConf ix dt'
--  apD     sc mut = apDr sc (mut dat
  --apD     (SystemConf ix dt) mut = SystemConf ix (mut dt)


--instance dtEngineConf :: DataTable EngineConf EngineConfD Record where
--  libProj (Library {engineConfLib}) = engineConfLib
--  idx     (EngineConf ix _) = ix
--  dat     (EngineConf _ dt) = dt
--  apI     (EngineConf ix dt) mut = EngineConf (mut ix) dt
--  apD     sc mut = sc
--
--instance dtUIConf :: DataTable UIConf UIConfD Record where
--  libProj (Library {uiConfLib}) = uiConfLib
--  idx     (UIConf ix _) = ix
--  dat     (UIConf _ dt) = dt
--  apI     (UIConf ix dt) mut = UIConf (mut ix) dt
--  apD     sc mut = sc
--
--instance dtPattern :: DataTable Pattern PatternD Record where
--  libProj (Library {patternLib}) = patternLib
--  idx     (Pattern ix _) = ix
--  dat     (Pattern _ dt) = dt
--  apI     (Pattern ix dt) mut = Pattern (mut ix) dt
--  apD     sc mut = sc
--
--instance dtModule :: DataTable Module ModuleD Record where
--  libProj (Library {moduleLib}) = moduleLib
--  idx     (Module ix _) = ix
--  dat     (Module _ dt) = dt
--  apI     (Module ix dt) mut = Module (mut ix) dt
--  apD     sc mut = sc
--
--instance dtComponent :: DataTable Component ComponentD Record where
--  libProj (Library {componentLib}) = componentLib
--  idx     (Component ix _) = ix
--  dat     (Component _ dt) = dt
--  apI     (Component ix dt) mut = Component (mut ix) dt
--  apD     sc mut = sc
--
--instance dtFamily :: DataTable Family FamilyD Record where
--  libProj (Library {familyLib}) = familyLib
--  idx     (Family ix _) = ix
--  dat     (Family _ dt) = dt
--  apI     (Family ix dt) mut = Family (mut ix) dt
--  apD     sc mut = sc
--
--instance dtImage :: DataTable Image ImageD Record where
--  libProj (Library {imageLib}) = imageLib
--  idx     (Image ix _) = ix
--  dat     (Image _ dt) = dt
--  apI     (Image ix dt) mut = Image (mut ix) dt
--  apD     sc mut = sc
--
--instance dtSection :: DataTable Section SectionD Record where
--  libProj (Library {sectionLib}) = sectionLib
--  idx     (Section ix _) = ix
--  dat     (Section _ dt) = dt
--  apI     (Section ix dt) mut = Section (mut ix) dt
--  apD     sc mut = sc


-- general crud

getLib :: forall a ad eff h r. (DataTable a ad r) => Library h -> String -> String -> EpiS eff h a
getLib lib name msg = do
  res <- liftEff $ peek (libProj lib) name
  -- dbg $ libProj lib
  -- dbg res
  case res of
    Just x -> pure x
    Nothing -> throwError $ msg <>" # Can't find in library: " <> name
  --fromJustE res (msg <>" # Can't find in library: " <> name)

modLib :: forall a ad eff h r. (DataTable a ad r) => Library h -> String -> a -> EpiS eff h Unit
modLib lib name new = do
  lift $ poke (libProj lib) name new # void

modLib' :: forall a ad eff h r. (DataTable a ad r) => Library h -> String -> (a -> a) -> EpiS eff h Unit
modLib' lib name mut = do
  lib' <- mut <$> getLib lib name "modLib' mutator"
  lift $ poke (libProj lib) name lib' # void

delLib :: forall a ad eff h r. (DataTable a ad r) => Library h -> a -> EpiS eff h Unit
delLib lib obj = do
  lift $ delete (libProj lib :: STStrMap h a) (idx obj).id # void


data LibSearch = LibSearch {flags::S.Set String, exclude::S.Set String, props::StrMap String}
emptySearch :: LibSearch
emptySearch = LibSearch {flags: S.empty, exclude: S.empty, props: empty}

searchLib :: forall a ad r eff h. (DataTable a ad r) => Library h -> LibSearch -> EpiS eff h (Array a)
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

--getUIConf :: forall eff h.  Library h -> String -> EpiS eff h UIConf
--getUIConf (Library {system: Nothing}) msg = do
--  throwError $ msg <> ": System not initialized"
--getUIConf lib@(Library {system: (Just system)}) msg = do
--  (SystemConfD systemConfD) <- getSystemConfD lib "getUIConf"
--  let name = (systemConfD.uiConf)
--  getLib lib name (msg <> ": Can't find uiConf - " <> name)
--
--getUIConfD :: forall eff h.  Library h -> String -> EpiS eff h UIConfD
--getUIConfD lib msg = do
--  (UIConf _ uiConfD) <- getUIConf lib msg
--  pure uiConfD
--
--getEngineConf :: forall eff h.  Library h -> String -> EpiS eff h EngineConf
--getEngineConf (Library {system: Nothing}) msg = do
--  throwError $ msg <> ": System not initialized"
--getEngineConf lib@(Library {system: (Just system)}) msg = do
--  (SystemConfD systemConfD) <- getSystemConfD lib "getEngineConf"
--  let name = (systemConfD.engineConf)
--  getLib lib name (msg <> ": Can't find engineConf - " <> name)
--
--getEngineConfD :: forall eff h.  Library h -> String -> EpiS eff h EngineConfD
--getEngineConfD lib msg = do
--  (EngineConf _ engineConfD) <- getEngineConf lib msg
--  pure engineConfD
