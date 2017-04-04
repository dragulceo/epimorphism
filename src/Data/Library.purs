module Data.Library where

import Prelude
--import Config (EpiS)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.Trans.Class (lift)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.StrMap (StrMap)
import Data.StrMap.ST (STStrMap, delete, peek, poke)

-- MAIN DATA TYPES
newtype FamilyRef    = FamilyRef String
newtype ModuleRef    = ModuleRef String
newtype ComponentRef = ComponentRef String
newtype PatternRef   = PatternRef String
newtype ImageRef     = ImageRef String

type Index = {
    id     :: String
  , parent :: String
  , flags  :: Set String
  , props  :: StrMap String
}

-- System
data SystemConf = SystemConf Index {
    initEngineConf :: String
  , initUIConf     :: String
  , initPattern    :: PatternRef
  , seed           :: String
}

data EngineConf = EngineConf Index {
    kernelDim            :: Int
  , fract                :: Int
  , numAuxBuffers        :: Int
  , audioAnalysisEnabled :: Boolean
  , audioBufferSize      :: Int
}

data UIConf = UIConf Index {
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

newtype Script = Script String
newtype Path = Path String
newtype Include = Include String
type CodeBlock = String

data Pattern = Pattern Index {
    vert            :: ModuleRef
  , main            :: ModuleRef
  , disp            :: ModuleRef
  , vertC           :: ComponentRef
  , mainC           :: ComponentRef
  , dispC           :: ComponentRef
  , defaultImageLib :: String
  , imageLib        :: String
  -- , 3d shit(everything between Engine & Modules)
}

data Family = Family Index {
    dim          :: Int
  , def_comp_ref :: ComponentRef
}

data Component = Component Index {
    family_ref  :: FamilyRef
  , def_mod_ref :: ModuleRef
  , children    :: StrMap FamilyRef
  , code        :: CodeBlock
  , includes    :: Array Include
}

data Module = Module Index {
    comp_ref :: ComponentRef
  , scripts  :: Array Script
  , modules  :: StrMap ModuleRef
  , par      :: StrMap Path
  , zn       :: Array Path
  , images   :: Array ImageRef
  , sub      :: StrMap String
 }

data Image = Image Index {
  path :: String
}

data Section = Section Index {
  values :: Array String
}

data Snapshot = Snapshot DateTime String


--- LIBRARY
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
}

--findLib :: forall a eff h. (Indexable a) => Library h -> String -> String -> EpiS eff h a
--findLib lib name msg = do
--  map <- lift $ peek (getLib lib) name
--  case map of
--    (Just ref) -> pure ref
--    Nothing  -> throwError $ msg <>" # Cant find in library: " <> name
--
--modLib :: forall a eff h. (Indexable a) => Library h -> String -> a -> EpiS eff h Unit
--modLib lib name new = do
--  lift $ poke (getLib lib) name new # void
--
--modLib' :: forall a eff h. (Indexable a) => Library h -> String -> (a -> a) -> EpiS eff h Unit
--modLib' lib name mut = do
--  old <- findLib lib name "modLib' mutator"
--  lift $ poke (getLib lib) name (mut old) # void
--
--delLib :: forall a eff h. (Indexable a) => Library h -> String -> EpiS eff h (STStrMap h a)
--delLib lib name = do
--  lift $ delete (getLib lib) name

--searchLib


class Indexable a where
   getLib :: forall h. Library h -> STStrMap h a
   index  :: a -> Index

instance indexSystemConf :: Indexable SystemConf where
  getLib (Library {systemConfLib}) = systemConfLib
  index  (SystemConf idx _) = idx

instance indexEngineConf :: Indexable EngineConf where
  getLib (Library {engineConfLib}) = engineConfLib
  index  (EngineConf idx _) = idx

instance indexUIConf :: Indexable UIConf where
  getLib (Library {uiConfLib}) = uiConfLib
  index  (UIConf idx _) = idx

instance indexPattern :: Indexable Pattern where
  getLib (Library {patternLib}) = patternLib
  index  (Pattern idx _) = idx

instance indexFamily :: Indexable Family where
  getLib (Library {familyLib}) = familyLib
  index  (Family idx _) = idx

instance indexComponent :: Indexable Component where
  getLib (Library {componentLib}) = componentLib
  index  (Component idx _) = idx

instance indexModule :: Indexable Module where
  getLib (Library {moduleLib}) = moduleLib
  index  (Module idx _) = idx

instance indexImage :: Indexable Image where
  getLib (Library {imageLib}) = imageLib
  index  (Image idx _) = idx

instance sectionIndex :: Indexable Section where
  getLib (Library {sectionLib}) = sectionLib
  index  (Section idx _) = idx
