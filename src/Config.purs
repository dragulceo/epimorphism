module Config where

import Data.Maybe (Maybe ())
import Data.Tuple (Tuple ())
import Data.StrMap (StrMap ())
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (ExceptT ())
import Graphics.WebGL.Types (WebGLProgram, WebGLTexture, WebGLFramebuffer, WebGLContext)
import Graphics.Canvas (Canvas)
import DOM (DOM)

type Epi eff a = ExceptT String (Eff (canvas :: Canvas, dom :: DOM | eff)) a

-- System
type SystemST = {
    lastTimeMS :: Maybe Number
  , frameNum :: Int
  , lastFpsTimeMS :: Maybe Number
  , fps :: Maybe Int
}

-- Engine
type EngineConf = {
    kernelDim :: Int
  , fract :: Int
}

type EngineST = {
    displayProg :: WebGLProgram
  , mainProg :: WebGLProgram
  , tex :: (Tuple WebGLTexture WebGLTexture)
  , fb :: (Tuple WebGLFramebuffer WebGLFramebuffer)
  , ctx :: WebGLContext
}

-- UI
type UIConf = {
    canvasId :: String
  , consoleId :: String
  -- , showFps :: Boolean
}

-- Pattern
newtype SubModules = SubModules (StrMap Module)

type Module = {
    name :: String
  , id :: String
  , family :: String
  , modules :: SubModules
  , par :: StrMap Number
  , zn :: Array Number
  , images :: Array String
  , sub :: StrMap String
}

type Pattern = {
    modules :: StrMap Module
  , scripts :: Array String
  -- , 3d shit
  , t :: Number
  , tPhase :: Number
  , tSpd :: Number
}
