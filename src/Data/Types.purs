module Data.Types where

import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.ST (ST)
import DOM (DOM)
import Data.DateTime (DateTime)
import Data.Set (Set) as S
import Data.StrMap (StrMap)
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
  , SchemaEntry SE_S    "flags"
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

engineConfSchema :: Schema
engineConfSchema = [
    SchemaEntry SE_I "kernelDim"
  , SchemaEntry SE_I "fract"
  , SchemaEntry SE_I "numAuxBuffers"
  , SchemaEntry SE_B "audioAnalysisEnabled"
  , SchemaEntry SE_I "audioBufferSize"
]

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
    vert            :: String -- ModuleRef
  , main            :: String -- ModuleRef
  , disp            :: String -- ModuleRef
  , vertC           :: String -- ComponentRef
  , mainC           :: String -- ComponentRef
  , dispC           :: String -- ComponentRef
  , includes        :: Array String -- REMOVE ME
  , defaultImageLib :: String
  , imageLib        :: String
  -- , 3d shit(everything between Engine & Modules)
}

patternSchema :: Schema
patternSchema = [
    SchemaEntry SE_St "vert"
  , SchemaEntry SE_St "main"
  , SchemaEntry SE_St "disp"
  , SchemaEntry SE_St "vertC"
  , SchemaEntry SE_St "mainC"
  , SchemaEntry SE_St "dispC"
  , SchemaEntry SE_A_St "includes"
  , SchemaEntry SE_St "defaultImageLib"
  , SchemaEntry SE_St "imageLib"
]

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

-- Pattern
data Module = Module Index ModuleD

type ModuleD = {
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
}

moduleSchema :: Schema
moduleSchema = [
    SchemaEntry SE_St "component"
  , SchemaEntry SE_A_St "scripts"
  , SchemaEntry SE_M_St "modules"
  , SchemaEntry SE_M_St "par"
  , SchemaEntry SE_A_St "zn"
  , SchemaEntry SE_A_St "images"
  , SchemaEntry SE_M_St "sub"
  , SchemaEntry SE_St "var"
  , SchemaEntry SE_St "dim"
  , SchemaEntry SE_St "libName"
  , SchemaEntry SE_St "family"
]


--data Module = Module Index ModuleD
--type ModuleD = {
--    comp_ref :: ComponentRef
--  , scripts  :: Array Script
--  , modules  :: StrMap ModuleRef
--  , par      :: StrMap Path
--  , zn       :: Array Path
--  , images   :: Array ImageRef
--  , sub      :: StrMap String
--}


data Image = Image Index ImageD
type ImageD = {
  path :: String
}


data Section = Section Index SectionD
type SectionD = {
  values :: Array String
}
