module Config where

import Prelude
import Data.Complex
import Data.Maybe (Maybe ())
import Data.Tuple (Tuple ())
import Data.StrMap (StrMap ())
import Control.Monad.ST (STRef)
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (ExceptT ())
import Graphics.WebGL.Types (WebGLProgram, WebGLTexture, WebGLFramebuffer, WebGLContext)
import Graphics.Canvas (Canvas)
import DOM (DOM)

import Data.String

type Epi eff a = ExceptT String (Eff (canvas :: Canvas, dom :: DOM | eff)) a

-- System
type SystemConf = {
    initEngineConf :: String
  , initUIConf :: String
  , initPattern :: String
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
  , moduleRefLib :: StrMap (STRef h Module)
  , componentLib :: StrMap Component
  , indexLib :: StrMap Index
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
  , scripts :: Array String
  , includes :: Array String
  -- , 3d shit
  , t :: Number
  , tPhase :: Number
  , tSpd :: Number
}


--SLib
type Component = {
    name :: String
  , family :: String
  , body :: String
}

type Index = {
    name :: String
  , lib :: Array String
}
