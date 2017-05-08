module Data.Types where

import Prelude
import Graphics.WebGL.Raw.Types as GLT
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.ST (ST, STRef)
import DOM (DOM)
import Data.Comp (CompOp, CompST)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Data.Set (Set, union) as S
import Data.StrMap (StrMap, empty)
import Data.StrMap.ST (STStrMap)
import Data.Tuple (Tuple)
import Graphics.Canvas (CANVAS)
import Graphics.WebGL.Types (WebGLFramebuffer, WebGLTexture, WebGLContext)

type Epi eff a = ExceptT String (Eff (canvas :: CANVAS, dom :: DOM | eff)) a
type EpiS eff h a = Epi (st :: ST h | eff) a

data SchemaEntryType = SE_St | SE_N | SE_I | SE_B | SE_S | SE_A_St | SE_A_Cx | SE_M_N | SE_M_St
data SchemaEntry = SchemaEntry SchemaEntryType String
type Schema = Array SchemaEntry

-- LIBRARY DATA TYPES
newtype FamilyRef    = FamilyRef String
newtype ModuleRef    = ModuleRef String
newtype ComponentRef = ComponentRef String
newtype PatternRef   = PatternRef String
newtype ImageRef     = ImageRef String

--newtype Script = Script String
newtype Path = Path String
newtype Include = Include String
newtype CodeBlock = CodeBlock String

data Snapshot = Snapshot DateTime String

type Index = {
    id     :: String
  , flags  :: S.Set String
  , props  :: StrMap String
  , orig   :: String
}

indexSchema :: Schema
indexSchema = [
    SchemaEntry SE_St   "id"
  , SchemaEntry SE_S    "flags"
  , SchemaEntry SE_M_St "props"
  , SchemaEntry SE_St   "orig"
]

-- System
data SystemConf = SystemConf Index SystemConfD
type SystemConfD = {
    engineConf :: String
  , uiConf     :: String
  , pattern    :: String --PatternRef
  , seed       :: String
  , debug      :: Boolean
}

systemConfSchema :: Schema
systemConfSchema = [
    SchemaEntry SE_St "engineConf"
  , SchemaEntry SE_St "uiConf"
  , SchemaEntry SE_St "pattern"
  , SchemaEntry SE_St "seed"
  , SchemaEntry SE_B  "debug"
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
  , seed            :: String -- ModuleRef
  , disp            :: String -- ModuleRef
  , defaultImageLib :: String
  , imageLib        :: String
}

patternSchema :: Schema
patternSchema = [
    SchemaEntry SE_St "vert"
  , SchemaEntry SE_St "main"
  , SchemaEntry SE_St "seed"
  , SchemaEntry SE_St "disp"
  , SchemaEntry SE_St "defaultImageLib"
  , SchemaEntry SE_St "imageLib"
]

data Family = Family Index FamilyD
type FamilyD = {
    var          :: String
  , dim          :: Int
  , default_comp :: ComponentRef
}

familySchema :: Schema
familySchema = [
    SchemaEntry SE_St "var"
  , SchemaEntry SE_N "dim"
  , SchemaEntry SE_St "default_comp"
]


data Component = Component Index ComponentD
type ComponentD = {
    family      :: String --FamilyRef
  , default_mod :: String --ModuleRef
  , children    :: StrMap String --StrMap FamilyRef
  , code        :: String --CodeBlock
  , includes    :: Array String --Array Include
}

componentSchema :: Schema
componentSchema = [
    SchemaEntry SE_St "family"
  , SchemaEntry SE_St "default_mod"
  , SchemaEntry SE_M_St "children"
  , SchemaEntry SE_St "code"
  , SchemaEntry SE_A_St "includes"
]

data Module = Module Index ModuleD
type ModuleD = {
    component :: String
  , scripts   :: Array String
  , modules   :: StrMap String
  , par       :: StrMap String
  , zn        :: Array String
  , images    :: Array String
  , sub       :: StrMap String
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
]

data Image = Image Index ImageD
type ImageD = {
  path :: String
}

imageSchema :: Schema
imageSchema = [
  SchemaEntry SE_St "path"
]

data Section = Section Index SectionD
type SectionD = {
  lib :: Array String
}

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

-- SYSTEM STATE
type SystemST h = {
    lastTimeMS :: Maybe Number
  , frameNum :: Int
  , lastFpsTimeMS :: Maybe Number
  , fps :: Maybe Int
  , t :: Number
  , paused :: Boolean
  , next :: Boolean
  , pauseAfterSwitch :: Boolean
  , version :: String
}

defaultSystemST :: forall h. SystemST h
defaultSystemST = {
    lastTimeMS: Nothing
  , frameNum: 0
  , lastFpsTimeMS: Nothing
  , fps: Nothing
  , t: 0.0
  , paused: false
  , next: false
  , pauseAfterSwitch: false
  , version: "1.0.0"
}

type EngineProfile = {
    os                :: String
  , browser           :: String
  , is_mobile         :: Boolean
  , angle             :: Boolean
  , max_texture_units :: Int
  , max_frag_uniforms :: Int
  , max_texture_size  :: Int
}

type EngineST = {
    tex :: Maybe (Tuple WebGLTexture WebGLTexture)
  , fb :: Maybe (Tuple WebGLFramebuffer WebGLFramebuffer)
  , seed :: Maybe (Tuple WebGLTexture WebGLFramebuffer)
  , auxTex :: Maybe (Array WebGLTexture)
  , currentImages :: Array String
  , audio :: Maybe (Tuple WebGLTexture AudioAnalyser)
  , ctx :: WebGLContext
  , empty :: GLT.TexImageSource
  , compQueue :: Array CompOp
  , curST  :: CompST
  , compST :: CompST
  , profile :: EngineProfile
}

type UIST = {
    incIdx :: StrMap Int
}

defaultUIST :: UIST
defaultUIST = {
    incIdx: empty
}

-- Script
data PMut = PMutNone | PMut PatternD (S.Set String)
instance mutSemi :: Semigroup PMut where
  append (PMut p0 s0) (PMut p1 s1) = PMut p0 (S.union s0 s1) -- sketchy if p0 != p1
  append PMutNone x = x
  append x PMutNone = x

-- sys -> time -> mid -> self -> args -> res
type ScriptFn eff h = STRef h (SystemST h) -> Library h -> Number -> String -> String -> StrMap String -> EpiS eff h ScriptRes

data ScriptConfig = ScriptConfig String
data ScriptRes = ScriptRes PMut (Maybe (StrMap String)) -- possible new root, possibly updated state
data Script = Script String Number (StrMap String)

-- MISC
foreign import data AudioAnalyser :: *
