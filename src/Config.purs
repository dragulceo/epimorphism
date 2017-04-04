module Config where

import Prelude
import Graphics.WebGL.Raw.Types as GLT
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.ST (STRef, ST)
import DOM (DOM)
import Data.Library (Library)
import Data.Maybe (Maybe(..))
import Data.Set (union, Set)
import Data.StrMap (StrMap, empty)
import Data.Tuple (Tuple)
import Graphics.Canvas (CANVAS)
import Graphics.WebGL.Types (WebGLProgram, WebGLTexture, WebGLFramebuffer, WebGLContext)

type Epi eff a = ExceptT String (Eff (canvas :: CANVAS, dom :: DOM | eff)) a
type EpiS eff h a = Epi (st :: ST h | eff) a

data SchemaEntryType = SE_St | SE_N | SE_I | SE_B | SE_S | SE_A_St | SE_A_Cx | SE_M_N | SE_M_St
data SchemaEntry = SchemaEntry SchemaEntryType String
type Schema = Array SchemaEntry

-- System
type SystemConf = {
    initEngineConf :: String
  , initUIConf     :: String
  , initPattern    :: String
  , host           :: String
  , seed           :: String
}

systemConfSchema :: Schema
systemConfSchema = [
    SchemaEntry SE_St "id"
  , SchemaEntry SE_St "flags"
  , SchemaEntry SE_St "props"
  , SchemaEntry SE_St "parent"
  , SchemaEntry SE_St "initEngineConf"
  , SchemaEntry SE_St "initUIConf"
  , SchemaEntry SE_St "initPattern"
  , SchemaEntry SE_St "seed"
]

type SystemST h = {
    lastTimeMS :: Maybe Number
  , frameNum :: Int
  , lastFpsTimeMS :: Maybe Number
  , fps :: Maybe Int
  , t :: Number
  , paused :: Boolean
  , pauseAfterSwitch :: Boolean
  , systemConfLib :: StrMap SystemConf
  , uiConfLib :: StrMap UIConf
  , engineConfLib :: StrMap EngineConf
  , patternLib :: StrMap Pattern
  , moduleLib :: StrMap Module
  , componentLib :: StrMap Component
  , indexLib :: StrMap Index
  , moduleRefPool :: StrMap (STRef h Module)
  , compPattern :: Maybe (STRef h Pattern) -- this is a bit weird
  , library :: Maybe (Library h)
}

defaultSystemST :: forall h. SystemST h
defaultSystemST = {
    lastTimeMS: Nothing
  , frameNum: 0
  , lastFpsTimeMS: Nothing
  , fps: Nothing
  , t: 0.0
  , paused: false
  , pauseAfterSwitch: false
  , systemConfLib: empty
  , uiConfLib: empty
  , engineConfLib: empty
  , patternLib: empty
  , moduleLib: empty
  , moduleRefPool: empty
  , componentLib: empty
  , indexLib: empty
  , compPattern: Nothing
  , library: Nothing
}

-- Engine
type EngineConf = {
    kernelDim :: Int
  , fract :: Int
  , numAuxBuffers :: Int
  , audioAnalysisEnabled :: Boolean
  , audioBufferSize :: Int
}

engineConfSchema :: Schema
engineConfSchema = [
    SchemaEntry SE_St "id"
  , SchemaEntry SE_St "flags"
  , SchemaEntry SE_St "props"
  , SchemaEntry SE_St "parent"
  , SchemaEntry SE_I "kernelDim"
  , SchemaEntry SE_I "fract"
  , SchemaEntry SE_I "numAuxBuffers"
  , SchemaEntry SE_B "audioAnalysisEnabled"
  , SchemaEntry SE_I "audioBufferSize"
]

foreign import data AudioAnalyser :: *
foreign import data UniformBindings :: *

type CompST = { auxImages :: Maybe (Array String),
                mainSrc :: Maybe String, dispSrc :: Maybe String, vertSrc :: Maybe String,
                mainProg :: Maybe WebGLProgram, dispProg :: Maybe WebGLProgram,
                mainUnif :: Maybe UniformBindings, dispUnif :: Maybe UniformBindings}
newCompST :: CompST
newCompST = {mainSrc: Nothing, dispSrc: Nothing, vertSrc: Nothing, auxImages: Nothing, mainProg: Nothing, dispProg: Nothing, mainUnif: Nothing, dispUnif: Nothing}

data CompOp = CompMainShader | CompDispShader | CompVertShader | CompMainProg | CompDispProg | CompFinish | CompStall

fullCompile :: Array CompOp
fullCompile = [CompVertShader, CompMainShader, CompMainProg, CompDispShader, CompDispProg, CompFinish]

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
    dispProg :: Maybe WebGLProgram
  , mainProg :: Maybe WebGLProgram
  , tex :: Maybe (Tuple WebGLTexture WebGLTexture)
  , fb :: Maybe (Tuple WebGLFramebuffer WebGLFramebuffer)
  , auxTex :: Maybe (Array WebGLTexture)
  , currentImages :: Maybe (Array String)
  , audio :: Maybe (Tuple WebGLTexture AudioAnalyser)
  , ctx :: WebGLContext
  , empty :: GLT.TexImageSource
  , compQueue :: Array CompOp
  , compST :: CompST
  , mainUnif :: Maybe UniformBindings
  , dispUnif :: Maybe UniformBindings
  , profile :: EngineProfile
}

-- UI
type UIConf = {
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
    SchemaEntry SE_St "id"
  , SchemaEntry SE_St "flags"
  , SchemaEntry SE_St "props"
  , SchemaEntry SE_St "parent"
  , SchemaEntry SE_St "canvasId"
  , SchemaEntry SE_St "consoleId"
  , SchemaEntry SE_St "fpsId"
  , SchemaEntry SE_B  "showFps"
  , SchemaEntry SE_St "windowState"
  , SchemaEntry SE_I  "uiUpdateFreq"
  , SchemaEntry SE_N  "keyboardSwitchSpd"
  , SchemaEntry SE_St "keySet"
  , SchemaEntry SE_St "uiCompLib"
]

type UIST = {
    incIdx :: StrMap Int
}

defaultUIST :: UIST
defaultUIST = {
    incIdx: empty
}


-- Pattern
type Module = {
    component :: String
  , family    :: String
  , flags     :: Set String
  , props     :: StrMap String
  , scripts   :: Array String
  , modules   :: StrMap String
  , par       :: StrMap String
  , zn        :: Array String
  , images    :: Array String
  , sub       :: StrMap String
  , var       :: String
  , dim       :: String
  , libName   :: String
}

componentSchema :: Schema
componentSchema = [
    SchemaEntry SE_St "id"
  , SchemaEntry SE_St "flags"
  , SchemaEntry SE_St "props"
  , SchemaEntry SE_St "parent"
  , SchemaEntry SE_St "family_ref"
  , SchemaEntry SE_St "def_mod_ref"
  , SchemaEntry SE_M_St "children"
  , SchemaEntry SE_St "code"
  , SchemaEntry SE_A_St "includes"
]

moduleSchema :: Schema
moduleSchema = [
    SchemaEntry SE_St "component"
  , SchemaEntry SE_St "family"
  , SchemaEntry SE_S "flags"
  , SchemaEntry SE_M_St "props"
  , SchemaEntry SE_A_St "scripts"
  , SchemaEntry SE_M_St "modules"
  , SchemaEntry SE_M_St "par"
  , SchemaEntry SE_A_St "zn"
  , SchemaEntry SE_A_St "images"
  , SchemaEntry SE_M_St "sub"
  , SchemaEntry SE_St "var"
  , SchemaEntry SE_St "dim"
  , SchemaEntry SE_St "libName"
]


type Pattern = {
    vert :: String
  , main :: String
  , disp :: String
  , flags :: Set String
  , props :: StrMap String
  , includes :: Array String
  , defaultImageLib :: String
  , imageLib :: String
  , tPhase :: Number
  , tSpd   :: Number
  -- , 3d shit
}

patternSchema :: Schema
patternSchema = [
    SchemaEntry SE_St "vert"
  , SchemaEntry SE_St "main"
  , SchemaEntry SE_St "disp"
  , SchemaEntry SE_S "flags"
  , SchemaEntry SE_M_St "props"
  , SchemaEntry SE_A_St "includes"
  , SchemaEntry SE_St "defaultImageLib"
  , SchemaEntry SE_St "imageLib"
  , SchemaEntry SE_N "tPhase"
  , SchemaEntry SE_N "tSpd"
]

data PMut = PMutNone | PMut Pattern (Set String)
instance mutSemi :: Semigroup PMut where
  append (PMut p0 s0) (PMut p1 s1) = PMut p0 (union s0 s1) -- sketchy if p0 != p1
  append PMutNone x = x
  append x PMutNone = x

-- Script
-- sys -> time -> mid -> idx -> args -> res
type ScriptFn eff h = STRef h (SystemST h) -> STRef h Pattern -> Number -> String -> Int -> StrMap String -> EpiS eff h ScriptRes

data ScriptConfig = ScriptConfig String
data ScriptRes = ScriptRes PMut (Maybe (StrMap String)) -- possible new root, possibly updated state
data Script = Script String Number (StrMap String)


--SLib
type Component = {
    name   :: String
  , family :: String
  , body   :: String
}

type Index = {
    name :: String
  , lib  :: Array String
}
