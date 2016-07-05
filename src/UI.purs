module UI where

import Prelude
import Command (command)
import Config (UIST, UIConf, EpiS, SystemST, SystemConf, Pattern, EngineST, EngineConf, defaultUIST)
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (STRef, readSTRef, newSTRef)
import KeyHandlers (keyHandler)
import Layout (initLayout)

foreign import registerEventHandler :: forall eff. (String -> Eff eff Unit) -> Eff eff Unit
foreign import registerKeyHandler :: forall eff. (String -> Eff eff String) -> Eff eff Unit
foreign import addGlobalEventListeners :: forall eff. (String -> Eff eff Unit) -> Eff eff Unit

-- PUBLIC
initUIST :: forall eff h. STRef h UIConf -> STRef h EngineConf -> STRef h EngineST -> STRef h Pattern -> STRef h SystemConf -> STRef h (SystemST h) -> EpiS eff h (STRef h UIST)
initUIST ucRef ecRef esRef pRef scRef ssRef = do
  let uiST = defaultUIST
  usRef  <- lift $ newSTRef uiST
  uiConf <- lift $ readSTRef ucRef

  initLayout uiConf uiST

  let handler = command ucRef usRef ecRef esRef pRef scRef ssRef
  lift $ addGlobalEventListeners handler
  lift $ registerEventHandler handler
  lift $ registerKeyHandler (keyHandler ucRef usRef)

  return usRef
