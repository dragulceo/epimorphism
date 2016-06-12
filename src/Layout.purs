module Layout where

import Prelude
import Config (SystemST, UIST, Epi, UIConf)
import Control.Monad (when, unless)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.Trans (lift)
import Data.DOM.Simple.Element (setInnerHTML, setStyleAttr, querySelector)
import Data.DOM.Simple.Unsafe.Element (HTMLElement)
import Data.DOM.Simple.Window (innerHeight, innerWidth, document, globalWindow)
import Data.Maybe (Maybe(Just, Nothing))

initLayout :: forall eff. UIConf -> UIST -> Epi eff Unit
initLayout uiConf uiST = do
  let window = globalWindow
  doc <- lift $ document window
  width  <- lift $ innerWidth window
  height <- lift $ innerHeight window

  unless uiConf.fullScreen do
    c2 <- findElt uiConf.canvasId
    lift $ setStyleAttr "width" (show (height - 10.0) ++ "px") c2
    lift $ setStyleAttr "height" (show (height - 11.0) ++ "px") c2

    console <- findElt uiConf.consoleId
    lift $ setStyleAttr "width" (show (width - height - 30.0) ++ "px") console
    lift $ setStyleAttr "height" (show (height - 21.0) ++ "px") console

  ds <- findElt uiConf.debugStateId
  case uiST.debugState of
    true -> do
      lift $ setStyleAttr "display" "block" ds
      return unit
    false -> do
      lift $ setStyleAttr "display" "none" ds
      return unit

  when uiConf.fullScreen do
    --lift $ requestFullScreen uiConf.canvasId
    c2 <- findElt uiConf.canvasId
    lift $ setStyleAttr "width" "100%" c2
    lift $ setStyleAttr "height" "100%" c2

-- hides malformed html issues
updateLayout :: forall eff h. UIConf -> UIST -> SystemST h -> Epi eff Unit
updateLayout uiConf uiST systemST = do
  case systemST.fps of
    (Just fps) -> showFps uiConf.fpsId fps
    Nothing -> return unit

--  when uiST.debugState do


findElt :: forall eff. String -> Epi eff HTMLElement
findElt id = do
  doc <- lift $ document globalWindow
  elt <- lift $ querySelector ("#" ++ id) doc

  case elt of
    (Just e) -> return e
    Nothing  -> throwError $ "couldn't find element: #" ++ id

showFps :: forall eff. String -> Int -> Epi eff Unit
showFps id fps = do
  let window = globalWindow
  doc <- lift $ document window
  fpsDiv <- findElt id
  lift $ setInnerHTML ((show fps) ++ "fps") fpsDiv
