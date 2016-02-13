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

import Config
import Command (command)
import JSUtil (unsafeLog)

foreign import registerEventHandler :: forall eff. (String -> Eff eff Unit) -> Eff eff Unit

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


initUIState :: forall h eff. (STRef h UIConf) -> (STRef h EngineConf) -> (STRef h EngineState) -> (STRef h Pattern) -> ExceptT String (Eff (st :: ST h, canvas :: Canvas, console :: CONSOLE, dom :: DOM  | eff)) Unit
initUIState ucRef ecRef esRef pRef = do
  uiConf <- lift $ readSTRef ucRef
  initLayout uiConf
  lift $ registerEventHandler (command ucRef ecRef esRef pRef)


initLayout :: forall eff. UIConf -> ExceptT String (Eff (canvas :: Canvas, console :: CONSOLE, dom :: DOM  | eff)) Unit
initLayout uiConf = do
  let window = globalWindow
  doc <- lift $ document window
  width  <- lift $ innerWidth window
  height <- lift $ innerHeight window

  --lift $ setCanvasWidth (height - 11.0) canvas
  --lift $ setCanvasHeight (height - 11.0) canvas

  Just c2 <- lift $ querySelector ("#" ++ uiConf.canvasId) doc
  lift $ setStyleAttr "width" (show (height - 10.0) ++ "px") c2
  lift $ setStyleAttr "height" (show (height - 11.0) ++ "px") c2

  -- this is unsafe
  Just console <- lift $ querySelector ("#" ++ uiConf.consoleId) doc
  lift $ setStyleAttr "width" (show (width - height - 30.0) ++ "px") console
  lift $ setStyleAttr "height" (show (height - 21.0) ++ "px") console


-- hackish
showFps :: forall eff. Int -> ExceptT String (Eff (canvas :: Canvas, console :: CONSOLE, dom :: DOM  | eff)) Unit
showFps fps = do
  let window = globalWindow
  doc <- lift $ document window
  Just fpsDiv <- lift $ querySelector ("#showfps") doc
  lift $ setInnerHTML ((show fps) ++ "fps") fpsDiv
