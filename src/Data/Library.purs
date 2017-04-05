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
import Data.Set (Set)
import Data.StrMap (StrMap)
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

type Index = {
    id     :: String
  , parent :: String
  , flags  :: Set String
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
type SystemConfD = {
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

type EngineConfD= {
    kernelDim            :: Int
  , fract                :: Int
  , numAuxBuffers        :: Int
  , audioAnalysisEnabled :: Boolean
  , audioBufferSize      :: Int
}
data EngineConf = EngineConf Index EngineConfD

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


newtype Script = Script String
newtype Path = Path String
newtype Include = Include String
type CodeBlock = String

data Pattern = Pattern Index {
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

data Family = Family Index {
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

data Module = Module Index {
    comp_ref :: ComponentRef
  , scripts  :: Array Script
  , modules  :: StrMap ModuleRef
  , par      :: StrMap Path
  , zn       :: Array Path
  , images   :: Array ImageRef
  , sub      :: StrMap String
 }

data Image = Image Index {
  path :: String
}

data Section = Section Index {
  values :: Array String
}

data Snapshot = Snapshot DateTime String


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

-- general crud

getLib :: forall a eff h. (Indexable a) => Library h -> String -> String -> EpiS eff h a
getLib lib name msg = do
  -- dbg $ inLib lib
  res <- liftEff $ peek (inLib lib) name
  -- <dbg res
  case res of
    Just x -> pure x
    Nothing -> throwError $ msg <>" # Can't find in library: " <> name
  --fromJustE res (msg <>" # Can't find in library: " <> name)

modLib :: forall a eff h. (Indexable a) => Library h -> String -> a -> EpiS eff h Unit
modLib lib name new = do
  lift $ poke (inLib lib) name new # void

modLib' :: forall a eff h. (Indexable a) => Library h -> String -> (a -> a) -> EpiS eff h Unit
modLib' lib name mut = do
  lib' <- mut <$> getLib lib name "modLib' mutator"
  lift $ poke (inLib lib) name lib' # void

delLib :: forall a eff h. (Indexable a) => Library h -> String -> EpiS eff h (STStrMap h a)
delLib lib name = do
  lift $ delete (inLib lib) name

--searchLib

-- specific finders

getSystemConfD :: forall eff h.  Library h -> String -> EpiS eff h SystemConfD
getSystemConfD (Library {system: Nothing}) msg = do
  throwError $ msg <> ": System not initialized"
getSystemConfD lib@(Library {system: (Just system)}) msg = do
  (SystemConf _ systemConfD) <- getLib lib system (msg <> ": Can't find system" <> system)
  pure systemConfD

getUIConfD :: forall eff h.  Library h -> String -> EpiS eff h UIConfD
getUIConfD (Library {system: Nothing}) msg = do
  throwError $ msg <> ": System not initialized"
getUIConfD lib@(Library {system: (Just system)}) msg = do
  systemConfD <- getSystemConfD lib "getUIConfD"
  let name = systemConfD.uiConf
  (UIConf _ uiConfD) <- getLib lib name (msg <> ": Can't find uiConf " <> name)
  pure uiConfD

getEngineConfD :: forall eff h.  Library h -> String -> EpiS eff h EngineConfD
getEngineConfD (Library {system: Nothing}) msg = do
  throwError $ msg <> ": System not initialized"
getEngineConfD lib@(Library {system: (Just system)}) msg = do
  systemConfD <- getSystemConfD lib "getEngineConfD"
  let name = systemConfD.engineConf
  (EngineConf _ engineConfD) <- getLib lib name (msg <> ": Can't find engineConf " <> name)
  pure engineConfD


class Indexable a where
  inLib :: forall h. Library h -> STStrMap h a
  index  :: a -> Index

instance indexSystemConf :: Indexable SystemConf where
  inLib (Library {systemConfLib}) = systemConfLib
  index  (SystemConf idx _) = idx

instance indexEngineConf :: Indexable EngineConf where
  inLib (Library {engineConfLib}) = engineConfLib
  index  (EngineConf idx _) = idx

instance indexUIConf :: Indexable UIConf where
  inLib (Library {uiConfLib}) = uiConfLib
  index  (UIConf idx _) = idx

instance indexPattern :: Indexable Pattern where
  inLib (Library {patternLib}) = patternLib
  index  (Pattern idx _) = idx

instance indexFamily :: Indexable Family where
  inLib (Library {familyLib}) = familyLib
  index  (Family idx _) = idx

instance indexComponent :: Indexable Component where
  inLib (Library {componentLib}) = componentLib
  index  (Component idx _) = idx

instance indexModule :: Indexable Module where
  inLib (Library {moduleLib}) = moduleLib
  index  (Module idx _) = idx

instance indexImage :: Indexable Image where
  inLib (Library {imageLib}) = imageLib
  index  (Image idx _) = idx

instance sectionIndex :: Indexable Section where
  inLib (Library {sectionLib}) = sectionLib
  index  (Section idx _) = idx
