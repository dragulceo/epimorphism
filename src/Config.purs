module Config where

import Prelude
import Data.Foreign
import Data.Foreign.Class
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
type SystemConf = {
    name :: String
  , initEngineConf :: String
  , initUIConf :: String
  , initPattern :: String
}

type SystemST = {
    lastTimeMS :: Maybe Number
  , frameNum :: Int
  , lastFpsTimeMS :: Maybe Number
  , fps :: Maybe Int
  , uiConfLib :: Array UIConf
  , engineConfLib :: Array EngineConf
  , patternLib :: Array Pattern
  , moduleLib :: Array Module
  , shaderLib :: StrMap String
  , componentLib :: StrMap String
  , libraryLib :: StrMap String
}

-- Engine
type EngineConf = {
    name :: String
  , kernelDim :: Int
  , fract :: Int
}

type EngineST = {
    displayProg :: WebGLProgram
  , mainProg :: WebGLProgram
  , tex :: (Tuple WebGLTexture WebGLTexture)
  , fb :: (Tuple WebGLFramebuffer WebGLFramebuffer)
  , ctx :: WebGLContext
}

data EngineConfD = EngineConfD EngineConf

instance showEngineConfD :: Show EngineConfD where
  show (EngineConfD o) = "{ name: " ++ show o.name ++ ", kernelDim: " ++ show o.kernelDim ++ ", fract: " ++ show o.fract ++ " }"

instance engineConfDIsForeign :: IsForeign EngineConfD where
  read value = do
    name      <- readProp "name" value
    kernelDim <- readProp "kernelDim" value
    fract     <- readProp "fract" value

    return $ EngineConfD {name: name, kernelDim: kernelDim, fract: fract}

unpackEngineConf :: EngineConfD -> EngineConf
unpackEngineConf (EngineConfD c) = c

-- UI
type UIConf = {
    name :: String
  , canvasId :: String
  , consoleId :: String
  -- , showFps :: Boolean
}

-- Pattern

class Shiz a where
  shiz :: a -> String

instance tmp :: Shiz (StrMap String) where
  shiz v = "asdf"


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
    name :: String
  , id :: String
  , modules :: StrMap Module
  , scripts :: Array String
  -- , 3d shit
  , t :: Number
  , tPhase :: Number
  , tSpd :: Number
}
