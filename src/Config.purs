module Config where

import Prelude
import Graphics.WebGL.Raw.Types as GLT
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.ST (STRef, ST)
import DOM (DOM)
import Data.Complex (Complex)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.StrMap (StrMap, empty)
import Data.Tuple (Tuple)
import Graphics.Canvas (Canvas)
import Graphics.WebGL.Types (WebGLProgram, WebGLTexture, WebGLFramebuffer, WebGLContext)

type Epi eff a = ExceptT String (Eff (canvas :: Canvas, dom :: DOM | eff)) a
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
    SchemaEntry SE_St "initEngineConf"
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
  , scriptLib :: StrMap Script
  , componentLib :: StrMap Component
  , indexLib :: StrMap Index
  , moduleRefPool :: StrMap (STRef h Module)
  , scriptRefPool :: StrMap (STRef h Script)
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
  , scriptLib: empty
  , scriptRefPool: empty
  , componentLib: empty
  , indexLib: empty
}

-- Engine
type EngineConf = {
    kernelDim :: Int
  , fract :: Int
  , numAux :: Int
  , audioAnalysisEnabled :: Boolean
  , audioBufferSize :: Int
}

engineConfSchema :: Schema
engineConfSchema = [
    SchemaEntry SE_I "kernelDim"
  , SchemaEntry SE_I "fract"
  , SchemaEntry SE_I "numAux"
  , SchemaEntry SE_B "audioAnalysisEnabled"
  , SchemaEntry SE_I "audioBufferSize"
]

foreign import data AudioAnalyser :: *

type EngineST = {
    dispProg :: Maybe WebGLProgram
  , mainProg :: Maybe WebGLProgram
  , tex :: Maybe (Tuple WebGLTexture WebGLTexture)
  , fb :: Maybe (Tuple WebGLFramebuffer WebGLFramebuffer)
  , aux :: Maybe (Array WebGLTexture)
  , auxImg :: Array String
  , audio :: Maybe (Tuple WebGLTexture AudioAnalyser)
  , ctx :: WebGLContext
  , empty :: GLT.TexImageSource
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
  , par       :: StrMap Number
  , zn        :: Array Complex
  , paths     :: StrMap String
  , images    :: Array String
  , sub       :: StrMap String
  , var       :: String
  , dim       :: String
  , libName   :: String
}

moduleSchema :: Schema
moduleSchema = [
    SchemaEntry SE_St "component"
  , SchemaEntry SE_St "family"
  , SchemaEntry SE_S "flags"
  , SchemaEntry SE_M_St "props"
  , SchemaEntry SE_A_St "scripts"
  , SchemaEntry SE_M_St "modules"
  , SchemaEntry SE_M_N "par"
  , SchemaEntry SE_A_Cx "zn"
  , SchemaEntry SE_M_St "paths"
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
  , tPhase :: Number
  , tSpd :: Number
  -- , 3d shit
}

patternSchema :: Schema
patternSchema = [
  SchemaEntry SE_St "vert",
  SchemaEntry SE_St "main",
  SchemaEntry SE_St "disp",
  SchemaEntry SE_S "flags",
  SchemaEntry SE_M_St "props",
  SchemaEntry SE_A_St "includes",
  SchemaEntry SE_N "tPhase",
  SchemaEntry SE_N "tSpd"
]

-- Script
-- sys -> self(name) -> time -> module -> self(ref) -> recompile
type ScriptFn eff h = STRef h (SystemST h) -> STRef h Pattern -> String -> Number -> String -> STRef h Script -> EpiS eff h Boolean
type Script = {
    fn     :: String
  , dt     :: StrMap String
  , flags  :: Set String
  , props  :: StrMap String
  , tPhase :: Number
}

scriptSchema :: Schema
scriptSchema = [
  SchemaEntry SE_St "fn",
  SchemaEntry SE_M_St "dt",
  SchemaEntry SE_S "flags",
  SchemaEntry SE_M_St "props",
  SchemaEntry SE_N "tPhase"
]

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
