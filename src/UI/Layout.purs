module Layout where

import Prelude
import Console (renderConsole)
import Control.Monad.Trans.Class (lift)
import Data.DOM.Simple.Element (classRemove, classAdd, setInnerHTML, setStyleAttr)
import Data.DOM.Simple.Window (innerHeight, innerWidth, document, globalWindow)
import Data.Library (getUIConfD)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Types (EpiS, Library, SystemST, UIST, EngineST)
import UIUtil (findElt)
import Util (lg)

initLayout :: forall eff h. UIST -> Library h -> EpiS eff h Unit
initLayout uiST lib = do
  uiConfD <- getUIConfD lib "initUIST"

  let window = globalWindow
  doc <- lift $ document window
  width  <- lift $ innerWidth window
  height <- lift $ innerHeight window

  canvas <- findElt uiConfD.canvasId
  cont <- findElt "container"
  menu <- findElt "menu"
  console <- findElt uiConfD.consoleId
  fps <- findElt uiConfD.fpsId

  lift $ classAdd "hide" console
  lift $ classAdd "hide" menu
  lift $ classAdd "hide" fps
  lift $ classRemove "fullWindow" cont

  when uiConfD.showFps do
    lift $ classRemove "hide" fps

  case uiConfD.windowState of
    "dev" -> do
      lift $ setStyleAttr "width" (show (height - 10.0) <> "px") canvas
      lift $ setStyleAttr "height" (show (height - 11.0) <> "px") canvas

      lift $ classRemove "hide" console
      lift $ setStyleAttr "width" (show (width - height - 30.0) <> "px") console
      lift $ setStyleAttr "height" (show (height - 21.0) <> "px") console
    _ -> do
      lift $ setStyleAttr "width" "" canvas
      lift $ setStyleAttr "height" "" canvas
      let ofs = (width - height) / -2.0
      lift $ setStyleAttr "top" (show ofs <> "px") canvas
      lift $ setStyleAttr "bottom" (show ofs <> "px") canvas

      lift $ classAdd "fullWindow" cont
      lift $ classRemove "hide" menu
      pure unit


-- hides malformed html issues
updateLayout :: forall eff h. UIST -> SystemST h -> EngineST -> Library h -> Boolean -> EpiS eff h Unit
updateLayout uiST systemST engineST lib force = do
  uiConfD <- getUIConfD lib "updateLayout"
  when (force ||
        (systemST.frameNum `mod` uiConfD.uiUpdateFreq == 0 && not systemST.paused)) do

    when uiConfD.showFps do
      case systemST.fps of
        (Just fps) -> do
          fpsDiv <- findElt uiConfD.fpsId
          lift $ setInnerHTML (show fps) fpsDiv
        Nothing -> pure unit

    when (uiConfD.windowState == "dev") do
      renderConsole uiST systemST engineST lib

    pure unit
