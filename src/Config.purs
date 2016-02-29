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
  , initUIConf :: String
  , initPattern :: String
}

defaultSystemConf :: SystemConf
defaultSystemConf = {
    initEngineConf: "default"
  , initUIConf:     "default"
  , initPattern:    "default"
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

type EngineST = {
    dispProg :: Maybe WebGLProgram
  , mainProg :: Maybe WebGLProgram
  , tex :: Maybe (Tuple WebGLTexture WebGLTexture)
  , fb :: Maybe (Tuple WebGLFramebuffer WebGLFramebuffer)
  , ctx :: WebGLContext
}

-- UI
type UIConf = {
    canvasId :: String
  , consoleId :: String
  -- , showFps :: Boolean
}

-- Pattern
type ModRef = String

type Module = {
    component :: String
  , flags :: StrMap String
  , scripts :: Array String
  , modules :: StrMap ModRef
  , par :: StrMap Number
  , zn :: Array Complex
  , images :: Array String
  , sub :: StrMap String
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
type ScriptFn eff h = Number -> StrMap String -> String ->  StrMap (STRef h Script) -> StrMap (STRef h Module) -> EpiS eff h  Unit
type Script = {
    fn    :: String
  , dt    :: StrMap String
  , mod   :: String
  , flags :: StrMap String
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
