module Config where

import Data.Maybe (Maybe ())
import Data.Tuple (Tuple ())
import Data.StrMap (StrMap ())
import Control.Monad.Eff (Eff)
import Graphics.WebGL.Types (WebGLProgram, WebGLTexture, WebGLFramebuffer, WebGLContext)

-- System
type SystemConf = {
    lastTimeMS :: Maybe Number
  , frameNum :: Int
}

-- Engine
type EngineConf = {
    kernelDim :: Int
  , fract :: Int
}

type EngineState = {
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
