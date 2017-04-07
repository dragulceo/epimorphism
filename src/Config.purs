module Config where

import Prelude
import Graphics.WebGL.Raw.Types as GLT
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.ST (STRef, ST)
import DOM (DOM)
import Data.Maybe (Maybe(..))
import Data.Set (union, Set)
import Data.StrMap (StrMap, empty)
import Data.Tuple (Tuple)
import Data.Types (Pattern, Schema, SchemaEntry(..), SchemaEntryType(..), PatternD)
import Graphics.Canvas (CANVAS)
import Graphics.WebGL.Types (WebGLProgram, WebGLTexture, WebGLFramebuffer, WebGLContext)

type Epi eff a = ExceptT String (Eff (canvas :: CANVAS, dom :: DOM | eff)) a
type EpiS eff h a = Epi (st :: ST h | eff) a

-- System
type SystemST h = {
    lastTimeMS :: Maybe Number
  , frameNum :: Int
  , lastFpsTimeMS :: Maybe Number
  , fps :: Maybe Int
  , t :: Number
  , paused :: Boolean
  , pauseAfterSwitch :: Boolean
  , patternLib :: StrMap Pattern
  , moduleLib :: StrMap Module
  , componentLib :: StrMap Component
  , indexLib :: StrMap Index
  , moduleRefPool :: StrMap (STRef h Module)
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
  , patternLib: empty
  , moduleLib: empty
  , moduleRefPool: empty
  , componentLib: empty
  , indexLib: empty
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


-- Pattern
type Module = {
    component :: String
  , family    :: String
  , flags     :: Set String
  , props     :: StrMap String
  , scripts   :: Array String
  , modules   :: StrMap String
  , par       :: StrMap String
  , zn        :: Array String
  , images    :: Array String
  , sub       :: StrMap String
  , var       :: String
  , dim       :: String
  , libName   :: String
}

moduleSchema :: Schema
moduleSchema = [
    SchemaEntry SE_St "component"
  , SchemaEntry SE_St "family"
  , SchemaEntry SE_S "flags"
  , SchemaEntry SE_M_St "props"
  , SchemaEntry SE_A_St "scripts"
  , SchemaEntry SE_M_St "modules"
  , SchemaEntry SE_M_St "par"
  , SchemaEntry SE_A_St "zn"
  , SchemaEntry SE_A_St "images"
  , SchemaEntry SE_M_St "sub"
  , SchemaEntry SE_St "var"
  , SchemaEntry SE_St "dim"
  , SchemaEntry SE_St "libName"
]

data PMut = PMutNone | PMut Pattern (Set String)
instance mutSemi :: Semigroup PMut where
  append (PMut p0 s0) (PMut p1 s1) = PMut p0 (union s0 s1) -- sketchy if p0 != p1
  append PMutNone x = x
  append x PMutNone = x

-- Script
-- sys -> time -> mid -> idx -> args -> res
type ScriptFn eff h = STRef h (SystemST h) -> STRef h Pattern -> Number -> String -> Int -> StrMap String -> EpiS eff h ScriptRes

data ScriptConfig = ScriptConfig String
data ScriptRes = ScriptRes PMut (Maybe (StrMap String)) -- possible new root, possibly updated state
data Script = Script String Number (StrMap String)


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
