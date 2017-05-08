module Data.Types where

import Prelude
import Graphics.WebGL.Raw.Types as GLT
import Control.Alt (class Alt, (<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.ST (ST, STRef)
import DOM (DOM)
import Data.DateTime (DateTime)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Maybe (Maybe(..))
import Data.Set (Set, union) as S
import Data.StrMap (StrMap, empty)
import Data.StrMap.ST (STStrMap)
import Data.Traversable (class Traversable, sequence, traverseDefault)
import Data.Tuple (Tuple)
import Graphics.Canvas (CANVAS)
import Graphics.WebGL.Types (WebGLFramebuffer, WebGLProgram, WebGLTexture, WebGLContext)
import Partial.Unsafe (unsafePartial)

type Epi eff a = ExceptT String (Eff (canvas :: CANVAS, dom :: DOM | eff)) a
type EpiS eff h a = Epi (st :: ST h | eff) a

data SchemaEntryType = SE_St | SE_N | SE_I | SE_B | SE_S | SE_A_St | SE_A_Cx | SE_M_N | SE_M_St
data SchemaEntry = SchemaEntry SchemaEntryType String
type Schema = Array SchemaEntry

-- LIBRARY DATA TYPES
newtype FamilyRef    = FamilyRef String
newtype ModuleRef    = ModuleRef String
newtype ComponentRef = ComponentRef String
newtype PatternRef   = PatternRef String
newtype ImageRef     = ImageRef String

--newtype Script = Script String
newtype Path = Path String
newtype Include = Include String
newtype CodeBlock = CodeBlock String

data Snapshot = Snapshot DateTime String

type Index = {
    id     :: String
  , flags  :: S.Set String
  , props  :: StrMap String
  , orig   :: String
}

indexSchema :: Schema
indexSchema = [
    SchemaEntry SE_St   "id"
  , SchemaEntry SE_S    "flags"
  , SchemaEntry SE_M_St "props"
  , SchemaEntry SE_St   "orig"
]

-- System
data SystemConf = SystemConf Index SystemConfD
type SystemConfD = {
    engineConf :: String
  , uiConf     :: String
  , pattern    :: String --PatternRef
  , seed       :: String
  , debug      :: Boolean
}

systemConfSchema :: Schema
systemConfSchema = [
    SchemaEntry SE_St "engineConf"
  , SchemaEntry SE_St "uiConf"
  , SchemaEntry SE_St "pattern"
  , SchemaEntry SE_St "seed"
  , SchemaEntry SE_B  "debug"
]

data EngineConf = EngineConf Index EngineConfD
type EngineConfD = {
    kernelDim            :: Int
  , fract                :: Int
  , numAuxBuffers        :: Int
  , audioAnalysisEnabled :: Boolean
  , audioBufferSize      :: Int
}

engineConfSchema :: Schema
engineConfSchema = [
    SchemaEntry SE_I "kernelDim"
  , SchemaEntry SE_I "fract"
  , SchemaEntry SE_I "numAuxBuffers"
  , SchemaEntry SE_B "audioAnalysisEnabled"
  , SchemaEntry SE_I "audioBufferSize"
]

data UIConf = UIConf Index UIConfD
type UIConfD = {
    canvasId          :: String
  , consoleId         :: String
  , fpsId             :: String
  , showFps           :: Boolean
  , windowState       :: String
  , uiUpdateFreq      :: Int
  , keyboardSwitchSpd :: Number
  , keySet            :: String
  , uiCompLib         :: String
}

uiConfSchema :: Schema
uiConfSchema = [
    SchemaEntry SE_St "canvasId"
  , SchemaEntry SE_St "consoleId"
  , SchemaEntry SE_St "fpsId"
  , SchemaEntry SE_B  "showFps"
  , SchemaEntry SE_St "windowState"
  , SchemaEntry SE_I  "uiUpdateFreq"
  , SchemaEntry SE_N  "keyboardSwitchSpd"
  , SchemaEntry SE_St "keySet"
  , SchemaEntry SE_St "uiCompLib"
]


data Pattern = Pattern Index PatternD
type PatternD = {
    vert            :: String -- ModuleRef
  , main            :: String -- ModuleRef
  , seed            :: String -- ModuleRef
  , disp            :: String -- ModuleRef
  , defaultImageLib :: String
  , imageLib        :: String
}

patternSchema :: Schema
patternSchema = [
    SchemaEntry SE_St "vert"
  , SchemaEntry SE_St "main"
  , SchemaEntry SE_St "seed"
  , SchemaEntry SE_St "disp"
  , SchemaEntry SE_St "defaultImageLib"
  , SchemaEntry SE_St "imageLib"
]

data Family = Family Index FamilyD
type FamilyD = {
    var          :: String
  , dim          :: Int
  , default_comp :: ComponentRef
}

familySchema :: Schema
familySchema = [
    SchemaEntry SE_St "var"
  , SchemaEntry SE_N "dim"
  , SchemaEntry SE_St "default_comp"
]


data Component = Component Index ComponentD
type ComponentD = {
    family      :: String --FamilyRef
  , default_mod :: String --ModuleRef
  , children    :: StrMap String --StrMap FamilyRef
  , code        :: String --CodeBlock
  , includes    :: Array String --Array Include
}

componentSchema :: Schema
componentSchema = [
    SchemaEntry SE_St "family"
  , SchemaEntry SE_St "default_mod"
  , SchemaEntry SE_M_St "children"
  , SchemaEntry SE_St "code"
  , SchemaEntry SE_A_St "includes"
]

data Module = Module Index ModuleD
type ModuleD = {
    component :: String
  , scripts   :: Array String
  , modules   :: StrMap String
  , par       :: StrMap String
  , zn        :: Array String
  , images    :: Array String
  , sub       :: StrMap String
}

moduleSchema :: Schema
moduleSchema = [
    SchemaEntry SE_St "component"
  , SchemaEntry SE_A_St "scripts"
  , SchemaEntry SE_M_St "modules"
  , SchemaEntry SE_M_St "par"
  , SchemaEntry SE_A_St "zn"
  , SchemaEntry SE_A_St "images"
  , SchemaEntry SE_M_St "sub"
]

data Image = Image Index ImageD
type ImageD = {
  path :: String
}

imageSchema :: Schema
imageSchema = [
  SchemaEntry SE_St "path"
]

data Section = Section Index SectionD
type SectionD = {
  lib :: Array String
}

data Library h = Library {
    systemConfLib :: STStrMap h SystemConf
  , engineConfLib :: STStrMap h EngineConf
  , uiConfLib     :: STStrMap h UIConf
  , patternLib    :: STStrMap h Pattern
  , familyLib     :: STStrMap h Family
  , componentLib  :: STStrMap h Component
  , moduleLib     :: STStrMap h Module
  , imageLib      :: STStrMap h Image
  , sectionLib    :: STStrMap h Section
  , system        :: Maybe String
}

-- SYSTEM STATE
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

type EngineST = {
    tex :: Maybe (Tuple WebGLTexture WebGLTexture)
  , fb :: Maybe (Tuple WebGLFramebuffer WebGLFramebuffer)
  , seed :: Maybe (Tuple WebGLTexture WebGLFramebuffer)
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


-- KERNELS & COMPILING
data Kernel = Seed | Main | Disp | Vert
instance showKernel :: Show Kernel where
  show Seed = "Seed"
  show Main = "Main"
  show Disp = "Disp"
  show Vert = "Vert"

data KMap a = KMap a a a a
derive instance kmapFunc :: Functor KMap

instance kmapFold :: Foldable KMap where
  foldMap mp (KMap x y z w) = (mp x) <> (mp y) <> (mp z) <> (mp w)
  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance kmapTrav :: Traversable KMap where
  sequence (KMap mx my mz mw) =
    let sl = sequence [mx, my, mz, mw]
    in (unsafePartial kmp) <$> sl
    where
      kmp :: forall a. Partial => Array a -> KMap a
      kmp [x, y, z, w] = (KMap x y z w)
  traverse f ta = traverseDefault f ta

--instance kmapAlt :: Alt a => Alt (KMap (a b)) where
--  alt (KMap x y z w) (KMap x' y' z' w') = (KMap (x <|> x') (y <|> y') (z <|> z') (w <|> w'))

--instance kmapAlt :: Alt a => Alt KMap where
altK :: forall a x. Alt a => KMap (a x) -> KMap (a x) -> KMap (a x)
altK (KMap x y z w) (KMap x' y' z' w') = (KMap (x <|> x') (y <|> y') (z <|> z') (w <|> w'))
infixr 0 altK as <||>

kGet :: forall a. KMap a -> Kernel -> a
kGet (KMap x _ _ _) Seed = x
kGet (KMap _ x _ _) Main = x
kGet (KMap _ _ x _) Disp = x
kGet (KMap _ _ _ x) Vert = x

kSet :: forall a. KMap a -> Kernel -> a -> KMap a
kSet (KMap x y z w) Seed x' = (KMap x' y z w)
kSet (KMap x y z w) Main y' = (KMap x y' z w)
kSet (KMap x y z w) Disp z' = (KMap x y z' w)
kSet (KMap x y z w) Vert w' = (KMap x y z w')

kAcs :: forall a r. KMap ({ seed :: a, main :: a, disp :: a, vert :: a | r } -> a)
kAcs = KMap (_.seed) (_.main) (_.disp) (_.vert)

kWrt :: forall a r. KMap a ->
        { seed :: a, main :: a, disp :: a, vert :: a | r } ->
        { seed :: a, main :: a, disp :: a, vert :: a | r }
kWrt (KMap x y z w) obj = obj { seed = x, main = y, disp = z, vert = w }

kNam :: KMap String
kNam = KMap "seed" "main" "disp" "vert"

type CompST = { src :: KMap (Maybe String), prog :: KMap (Maybe WebGLProgram),
                unif :: KMap (Maybe UniformBindings), aux :: KMap (Maybe (Array String))}

altCST :: CompST -> CompST -> CompST
altCST {src: s0, prog: p0, unif: u0, aux: a0} {src: s1, prog: p1, unif: u1, aux: a1} =
  {src: s0 <||> s1, prog: p0 <||> p1, unif: u0 <||> u1, aux: a0 <||> a1}
infixr 0 altCST as <|||>

newCompST :: CompST
newCompST = { src: KMap Nothing Nothing Nothing Nothing,
              prog: KMap Nothing Nothing Nothing Nothing,
              unif: KMap Nothing Nothing Nothing Nothing,
              aux: KMap Nothing Nothing Nothing Nothing  }

data CompOp = CompShader Kernel | CompProg Kernel | CompFinish | CompStall
instance showCompOp :: Show CompOp where
  show (CompShader k) = "CompShader: " <> (show k)
  show (CompProg k)   = "CompProg: "   <> (show k)
  show (CompFinish)   = "CompFinish"
  show (CompStall)    = "CompStall"

fullCompile :: Array CompOp
fullCompile = [CompShader Seed, CompShader Main, CompShader Disp, CompShader Vert,
               CompProg Seed, CompProg Main, CompProg Disp, CompFinish]



-- Script
data PMut = PMutNone | PMut PatternD (S.Set String)
instance mutSemi :: Semigroup PMut where
  append (PMut p0 s0) (PMut p1 s1) = PMut p0 (S.union s0 s1) -- sketchy if p0 != p1
  append PMutNone x = x
  append x PMutNone = x

-- sys -> time -> mid -> self -> args -> res
type ScriptFn eff h = STRef h (SystemST h) -> Library h -> Number -> String -> String -> StrMap String -> EpiS eff h ScriptRes

data ScriptConfig = ScriptConfig String
data ScriptRes = ScriptRes PMut (Maybe (StrMap String)) -- possible new root, possibly updated state
data Script = Script String Number (StrMap String)

-- MISC
foreign import data AudioAnalyser :: *
foreign import data UniformBindings :: *
