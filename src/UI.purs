module UI where

import Prelude
import Command (command)
import Config (UIST, SystemST, EngineST, defaultUIST)
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (STRef, readSTRef, newSTRef)
import Data.Library (Library, getPatternD, getUIConfD)
import Data.Types (EpiS)
import KeyHandlers (keyHandler)
import Layout (initLayout)
import System (loadLib)
import Util (Now)

foreign import registerEventHandler :: forall eff. (String -> Eff eff Unit) -> Eff eff Unit
foreign import registerKeyHandler :: forall eff. (String -> Eff eff String) -> Eff eff Unit
foreign import addGlobalEventListeners :: forall eff. (String -> Eff eff Unit) -> Eff eff Unit
foreign import registerAuxImages :: forall eff. Array String -> Eff eff Unit
foreign import doneLoading :: forall eff. Eff eff Unit

-- PUBLIC
initUIST :: forall eff h. STRef h EngineST -> STRef h (SystemST h) -> Library h -> EpiS (now :: Now | eff) h (STRef h UIST)
initUIST esRef ssRef lib = do
  let uiST = defaultUIST
  uiConfD  <- getUIConfD lib "initUIST uiConf"
  patternD <- getPatternD lib "initUIST pattern"

  usRef   <- lift $ newSTRef uiST

  initLayout uiST lib

  let handler = command usRef esRef ssRef lib
  lift $ addGlobalEventListeners handler
  lift $ registerEventHandler handler
  lift $ registerKeyHandler (keyHandler usRef lib)

  systemST <- lift $ readSTRef ssRef

  imgLib <- loadLib patternD.imageLib systemST.indexLib "all_images registerAux"

  lift $ registerAuxImages imgLib.lib

  lift $ doneLoading

  pure usRef
