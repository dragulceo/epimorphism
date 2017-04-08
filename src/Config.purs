module Config where

import Prelude
import Graphics.WebGL.Raw.Types as GLT
import Control.Monad.ST (STRef)
import Data.Library (Library)
import Data.Maybe (Maybe(..))
import Data.Set (union, Set)
import Data.StrMap (StrMap, empty)
import Data.Tuple (Tuple)
import Data.Types (EpiS, PatternD)
import Graphics.WebGL.Types (WebGLProgram, WebGLTexture, WebGLFramebuffer, WebGLContext)

-- System
type SystemST h = {
    lastTimeMS :: Maybe Number
  , frameNum :: Int
  , lastFpsTimeMS :: Maybe Number
  , fps :: Maybe Int
  , t :: Number
  , paused :: Boolean
  , pauseAfterSwitch :: Boolean
  , componentLib :: StrMap Component
  , compPattern :: Maybe PatternD
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
  , componentLib: empty
  , compPattern: Nothing
}

foreign import data AudioAnalyser :: *
foreign import data UniformBindings :: *

type CompST = { auxImages :: Maybe (Array String),
                mainSrc :: Maybe String, dispSrc :: Maybe String, vertSrc :: Maybe String,
                mainProg :: Maybe WebGLProgram, dispProg :: Maybe WebGLProgram,
                mainUnif :: Maybe UniformBindings, dispUnif :: Maybe UniformBindings}
newCompST :: CompST
newCompST = {mainSrc: Nothing, dispSrc: Nothing, vertSrc: Nothing, auxImages: Nothing, mainProg: Nothing, dispProg: Nothing, mainUnif: Nothing, dispUnif: Nothing}

data CompOp = CompMainShader | CompDispShader | CompVertShader | CompMainProg | CompDispProg | CompFinish | CompStall

fullCompile :: Array CompOp
fullCompile = [CompVertShader, CompMainShader, CompMainProg, CompDispShader, CompDispProg, CompFinish]

type EngineProfile = {
    os                :: String
  , browser           :: String
  , is_mobile         :: Boolean
  , angle             :: Boolean
  , max_texture_units :: Int
  , max_frag_uniforms :: Int
  , max_texture_size  :: Int
}

type EngineST = {
    dispProg :: Maybe WebGLProgram
  , mainProg :: Maybe WebGLProgram
  , tex :: Maybe (Tuple WebGLTexture WebGLTexture)
  , fb :: Maybe (Tuple WebGLFramebuffer WebGLFramebuffer)
  , auxTex :: Maybe (Array WebGLTexture)
  , currentImages :: Maybe (Array String)
  , audio :: Maybe (Tuple WebGLTexture AudioAnalyser)
  , ctx :: WebGLContext
  , empty :: GLT.TexImageSource
  , compQueue :: Array CompOp
  , compST :: CompST
  , mainUnif :: Maybe UniformBindings
  , dispUnif :: Maybe UniformBindings
  , profile :: EngineProfile
}

type UIST = {
    incIdx :: StrMap Int
}

defaultUIST :: UIST
defaultUIST = {
    incIdx: empty
}

data PMut = PMutNone | PMut PatternD (Set String)
instance mutSemi :: Semigroup PMut where
  append (PMut p0 s0) (PMut p1 s1) = PMut p0 (union s0 s1) -- sketchy if p0 != p1
  append PMutNone x = x
  append x PMutNone = x

-- Script
-- sys -> time -> mid -> idx -> args -> res
type ScriptFn eff h = STRef h (SystemST h) -> Library h -> Number -> String -> Int -> StrMap String -> EpiS eff h ScriptRes

data ScriptConfig = ScriptConfig String
data ScriptRes = ScriptRes PMut (Maybe (StrMap String)) -- possible new root, possibly updated state
data Script = Script String Number (StrMap String)


--SLib
type Component = {
    name   :: String
  , family :: String
  , body   :: String
}
