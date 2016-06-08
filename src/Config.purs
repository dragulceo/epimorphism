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

-- System
type SystemConf = {
    initEngineConf :: String
  , initUIConf     :: String
  , initPattern    :: String
  , host           :: String
}

defaultSystemConf :: SystemConf
defaultSystemConf = {
    initEngineConf: "default"
  , initUIConf:     "default"
  , initPattern:    "default"
  , host:           "http://localhost:8000"
}

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
  , testObjLib :: StrMap TestObj
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
  , testObjLib: empty
  , mainRef: ""
}

-- Engine
type EngineConf = {
    kernelDim :: Int
  , fract :: Int
}

defaultEngineConf :: EngineConf
defaultEngineConf = {
    kernelDim: 1024
  , fract: 3
}

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

defaultUIConf :: UIConf
defaultUIConf = {
    canvasId:   "glcanvas"
  , consoleId:  "console"
  , fullScreen: false
  , keyboardSwitchSpd: 1.0
}

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

-- Script
-- sys -> self(name) -> time -> module -> self(ref)
type ScriptFn eff h = STRef h (SystemST h) -> String -> Number -> String -> STRef h Script -> EpiS eff h Boolean
type Script = {
    fn     :: String
  , dt     :: StrMap String
  , mid    :: Maybe String
  , flags  :: StrMap String
  , tPhase :: Number
}

defaultScript :: Script
defaultScript = {
    fn:     "null"
  , dt:     empty
  , mid:    Nothing
  , flags:  empty
  , tPhase: 0.0
}

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


data SchemaEntryType = SE_St | SE_N | SE_I | SE_B | SE_S | SE_A_St | SE_A_Cx | SE_M_N | SE_M_St
data SchemaEntry = SchemaEntry SchemaEntryType String
type Schema = Array SchemaEntry

systemConfSchema :: Schema
systemConfSchema = [
  SchemaEntry SE_St "initEngineConf",
  SchemaEntry SE_St "initUIConf",
  SchemaEntry SE_St "initPattern"
]

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
