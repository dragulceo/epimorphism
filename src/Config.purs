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
}

systemConfSchema :: Schema
systemConfSchema = [
  SchemaEntry SE_St "initEngineConf",
  SchemaEntry SE_St "initUIConf",
  SchemaEntry SE_St "initPattern"
]

type SystemST h = {
    lastTimeMS :: Maybe Number
  , frameNum :: Int
  , lastFpsTimeMS :: Maybe Number
  , fps :: Maybe Int
  , t :: Number
  , systemConfLib :: StrMap SystemConf
  , uiConfLib :: StrMap UIConf
  , engineConfLib :: StrMap EngineConf
  , patternLib :: StrMap Pattern
  , moduleLib :: StrMap Module
  , moduleRefPool :: StrMap (STRef h Module)
  , scriptLib :: StrMap Script
  , scriptRefPool :: StrMap (STRef h Script)
  , componentLib :: StrMap Component
  , indexLib :: StrMap Index
  , mainRef :: String
}

defaultSystemST :: forall h. SystemST h
defaultSystemST = {
    lastTimeMS: Nothing
  , frameNum: 0
  , lastFpsTimeMS: Nothing
  , fps: Nothing
  , t: 0.0
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
  , mainRef: ""
}

-- Engine
type EngineConf = {
    kernelDim :: Int
  , fract :: Int
}

engineConfSchema :: Schema
engineConfSchema = [
  SchemaEntry SE_I "kernelDim",
  SchemaEntry SE_I "fract"
]

type EngineST = {
    dispProg :: Maybe WebGLProgram
  , mainProg :: Maybe WebGLProgram
  , tex :: Maybe (Tuple WebGLTexture WebGLTexture)
  , fb :: Maybe (Tuple WebGLFramebuffer WebGLFramebuffer)
  , aux :: Maybe (Array WebGLTexture)
  , auxN :: Int
  , ctx :: WebGLContext
  , empty :: GLT.TexImageSource
}

-- UI
type UIConf = {
    canvasId          :: String
  , consoleId         :: String
  , fullScreen        :: Boolean
  , keyboardSwitchSpd :: Number
  -- , showFps :: Boolean
}

uiConfSchema :: Schema
uiConfSchema = [
  SchemaEntry SE_St "canvasId",
  SchemaEntry SE_St "consoleId",
  SchemaEntry SE_B  "fullScreen",
  SchemaEntry SE_N  "keyboardSwitchSpd"
]

type UIST = {
    incIdx :: StrMap Int
}

defaultUIST :: UIST
defaultUIST = {
    incIdx: empty
}


-- Pattern
type ModRef = String

type Module = {
    component :: String
  , flags     :: StrMap String
  , scripts   :: Array String
  , modules   :: StrMap ModRef
  , par       :: StrMap Number
  , zn        :: Array Complex
  , images    :: Array String
  , sub       :: StrMap String
  , var       :: String
}

moduleSchema :: Schema
moduleSchema = [
  SchemaEntry SE_St "component",
  SchemaEntry SE_M_St "flags",
  SchemaEntry SE_A_St "scripts",
  SchemaEntry SE_M_St "modules",
  SchemaEntry SE_M_N "par",
  SchemaEntry SE_A_Cx "zn",
  SchemaEntry SE_A_St "images",
  SchemaEntry SE_M_St "sub",
  SchemaEntry SE_St "var"
]

type Pattern = {
    vert :: ModRef
  , main :: ModRef
  , disp :: ModRef
  , flags :: StrMap String
  , includes :: Array String
  -- , 3d shit
  , tPhase :: Number
  , tSpd :: Number
}

patternSchema :: Schema
patternSchema = [
  SchemaEntry SE_St "vert",
  SchemaEntry SE_St "main",
  SchemaEntry SE_St "disp",
  SchemaEntry SE_M_St "flags",
  SchemaEntry SE_A_St "includes",
  SchemaEntry SE_N "tPhase",
  SchemaEntry SE_N "tSpd"
]

-- Script
-- sys -> self(name) -> time -> module -> self(ref)
type ScriptFn eff h = STRef h (SystemST h) -> String -> Number -> String -> STRef h Script -> EpiS eff h Boolean
type Script = {
    fn     :: String
  , dt     :: StrMap String
  , mid    :: String
  , flags  :: StrMap String
  , tPhase :: Number
}

scriptSchema :: Schema
scriptSchema = [
  SchemaEntry SE_St "fn",
  SchemaEntry SE_M_St "dt",
  SchemaEntry SE_St "mid",
  SchemaEntry SE_M_St "flags",
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



-- trash

type TestObj = {
    t_str :: String
  , t_num :: Number
  , t_int :: Int
  , t_bool :: Boolean
  , t_set :: Set String
  , t_ast :: Array String
  , t_acx :: Array Complex
  , t_mn  :: StrMap Number
  , t_mst :: StrMap Int
}

testObjSchema :: Schema
testObjSchema = [
    SchemaEntry SE_St "t_str"
  , SchemaEntry SE_N "t_num"
  , SchemaEntry SE_I "t_int"
  , SchemaEntry SE_B "t_bool"
  , SchemaEntry SE_S "t_set"
  , SchemaEntry SE_A_St "t_ast"
  , SchemaEntry SE_A_Cx "t_acx"
  , SchemaEntry SE_M_N "t_mn"
  , SchemaEntry SE_M_St "t_mst"
]


defaultSystemConf :: SystemConf
defaultSystemConf = {
    initEngineConf: "default"
  , initUIConf:     "default"
  , initPattern:    "default"
  , host:           "http://localhost:8000"
}

defaultEngineConf :: EngineConf
defaultEngineConf = {
    kernelDim: 1024
  , fract: 3
}

defaultUIConf :: UIConf
defaultUIConf = {
    canvasId:   "glcanvas"
  , consoleId:  "console"
  , fullScreen: false
  , keyboardSwitchSpd: 1.0
}

defaultScript :: Script
defaultScript = {
    fn:     "null"
  , dt:     empty
  , mid:    ""
  , flags:  empty
  , tPhase: 0.0
}
