module Config where

import Prelude
import Data.Complex
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple ())
import Data.StrMap (StrMap (), empty)
import Control.Monad.ST (STRef, ST)
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (ExceptT ())
import Graphics.WebGL.Types (WebGLProgram, WebGLTexture, WebGLFramebuffer, WebGLContext)
import Graphics.Canvas (Canvas)
import DOM (DOM)

import Data.String

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
}

defaultSystemST :: forall h. SystemST h
defaultSystemST = {
    lastTimeMS: Nothing
  , frameNum: 0
  , lastFpsTimeMS: Nothing
  , fps: Nothing
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
  , t :: Number
  , tPhase :: Number
  , tSpd :: Number
}

-- Script
-- sys -> self(name) -> time -> module -> self(ref)
type ScriptFn eff h = STRef h (SystemST h) -> String -> Number -> String -> STRef h Script -> EpiS eff h Boolean
type Script = {
    fn    :: String
  , dt    :: StrMap String
  , mid   :: Maybe String
  , flags :: StrMap String
}

defaultScript :: Script
defaultScript = {
    fn: "null"
  , dt: empty
  , mid: Nothing
  , flags: empty
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
