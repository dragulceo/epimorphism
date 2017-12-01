module Data.System where

import Graphics.WebGL.Raw.Types as GLT
import Data.Comp (CompOp, CompST)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty)
import Data.Tuple (Tuple)
import Graphics.WebGL.Types (WebGLFramebuffer, WebGLTexture, WebGLContext)

foreign import data AudioAnalyser :: *

type SystemST h = {
    lastTimeMS :: Maybe Number
  , frameNum :: Int
  , lastFpsTimeMS :: Maybe Number
  , fps :: Maybe Int
  , t :: Number
  , paused :: Boolean
  , next :: Boolean
  , pauseAfterSwitch :: Boolean
  , version :: String
}

defaultSystemST :: forall h. SystemST h
defaultSystemST = {
    lastTimeMS: Nothing
  , frameNum: 0
  , lastFpsTimeMS: Nothing
  , fps: Nothing
  , t: 0.0
  , paused: false
  , next: false
  , pauseAfterSwitch: false
  , version: "1.0.0"
}

type EngineProfile = {
    os                :: String
  , browser           :: String
  , is_mobile         :: Boolean
  , angle             :: Boolean
  , max_texture_units :: Int
  , max_frag_uniforms :: Int
  , max_texture_size  :: Int
}

type KernelBuffer = Tuple WebGLTexture WebGLFramebuffer
type EngineST = {
    fb :: Maybe (Tuple KernelBuffer KernelBuffer)
  , seed :: Maybe (KernelBuffer)
  , auxTex :: Maybe (Array WebGLTexture)
  , currentImages :: Array String
  , audio :: Maybe (Tuple WebGLTexture AudioAnalyser)
  , ctx :: WebGLContext
  , empty :: GLT.TexImageSource
  , compQueue :: Array CompOp
  , curST  :: CompST
  , compST :: CompST
  , profile :: EngineProfile
}

type UIST = {
    incIdx :: StrMap Int
}

defaultUIST :: UIST
defaultUIST = {
    incIdx: empty
}
