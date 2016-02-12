module UI where

import Prelude

import Data.Maybe (Maybe (Just))
import Data.Int

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (runExceptT, lift, ExceptT ())
import Control.Monad.ST

import DOM (DOM)
import Graphics.Canvas
import Data.DOM.Simple.Window
import Data.DOM.Simple.Document
import Data.DOM.Simple.Element

import Engine (EngineConf, EngineState, resizeViewport)
import Pattern (Pattern)
import System (SystemConf)
import JSUtil (unsafeLog)

foreign import registerEventHandler :: forall eff. (String -> Eff ( | eff) Unit)
  -> Eff ( | eff) Unit
--foreign import onkeydown ::  forall eff. (Event -> Eff (webgl :: WebGl | eff) Unit)
--    -> Eff (webgl :: WebGl | eff) Unit
--foreign import onKeyUp ::  forall eff. (Event -> Eff (webgl :: WebGl | eff) Unit)
--    -> Eff (webgl :: WebGl | eff) Unit
--foreign import eventGetKeyCode :: Event -> Int


-- DATA
type UIConf = {
    canvasId :: String
  , consoleId :: String
}

defaultUIConf :: UIConf
defaultUIConf = {
    canvasId: "glcanvas"
  , consoleId: "console"
}

-- PUBLIC
loadUIConf :: forall eff. String -> ExceptT String (Eff eff) UIConf
loadUIConf name = do
  let uiConf = defaultUIConf
  return uiConf


initUIState :: forall h eff. (STRef h UIConf) -> (STRef h SystemConf) -> (STRef h EngineConf) -> (STRef h EngineState) -> (STRef h Pattern) -> ExceptT String (Eff (st :: ST h, canvas :: Canvas, console :: CONSOLE, dom :: DOM  | eff)) Unit
initUIState ucRef scRef ecRef esRef pRef = do
  uiConf      <- lift $ readSTRef ucRef
  systemConf  <- lift $ readSTRef scRef
  engineConf  <- lift $ readSTRef ecRef
  engineState <- lift $ readSTRef esRef
  pattern     <- lift $ readSTRef pRef

  initLayout uiConf engineState


initLayout :: forall eff. UIConf -> EngineState -> ExceptT String (Eff (canvas :: Canvas, console :: CONSOLE, dom :: DOM  | eff)) Unit
initLayout uiConf engineState = do
  -- this is unsafe
  Just canvas <- liftEff $ getCanvasElementById uiConf.canvasId

  let window = globalWindow
  doc <- lift $ document window
  width  <- lift $ innerWidth window
  height <- lift $ innerHeight window

  lift $ setCanvasWidth (height - 11.0) canvas
  lift $ setCanvasHeight (height - 11.0) canvas

  -- this is unsafe
  Just console <- lift $ querySelector ("#" ++ uiConf.consoleId) doc
  lift $ setStyleAttr "width" (show (width - height - 30.0) ++ "px") console
  lift $ setStyleAttr "height" (show (height - 21.0) ++ "px") console
  --lift $ unsafeLog $ console

  --case (fromNumber height) of
    --(Just h) -> resizeViewport engineState h h
