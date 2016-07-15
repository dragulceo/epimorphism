module Layout where

import Prelude
import Config (Script, scriptSchema, EpiS, moduleSchema, Module, Pattern, SystemST, UIST, Epi, UIConf)
import Control.Monad (when)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (readSTRef, STRef)
import Control.Monad.Trans (lift)
import Data.DOM.Simple.Element (classRemove, classAdd, setInnerHTML, setStyleAttr, querySelector)
import Data.DOM.Simple.Unsafe.Element (HTMLElement)
import Data.DOM.Simple.Window (innerHeight, innerWidth, document, globalWindow)
import Data.Maybe (Maybe(Just, Nothing))
import Data.StrMap (foldM, StrMap)
import Data.String (joinWith, trim, split, replace)
import Data.String.Regex (match, noFlags, regex)
import Data.Traversable (traverse)
import Serialize (unsafeSerialize)
import System (loadLib)
import Util (lg, indentLines)

initLayout :: forall eff. UIConf -> UIST -> Epi eff Unit
initLayout uiConf uiST = do
  let window = globalWindow
  doc <- lift $ document window
  width  <- lift $ innerWidth window
  height <- lift $ innerHeight window

  canvas <- findElt uiConf.canvasId
  cont <- findElt "container"
  menu <- findElt "menu"
  console <- findElt uiConf.consoleId
  fps <- findElt uiConf.fpsId

  lift $ classAdd "hide" console
  lift $ classAdd "hide" menu
  lift $ classAdd "hide" fps
  lift $ classRemove "fullWindow" cont

  when uiConf.showFps do
    lift $ classRemove "hide" fps

  case uiConf.windowState of
    "dev" -> do
      lift $ setStyleAttr "width" (show (height - 10.0) ++ "px") canvas
      lift $ setStyleAttr "height" (show (height - 11.0) ++ "px") canvas

      lift $ classRemove "hide" console
      lift $ setStyleAttr "width" (show (width - height - 30.0) ++ "px") console
      lift $ setStyleAttr "height" (show (height - 21.0) ++ "px") console
    _ -> do
      lift $ setStyleAttr "width" "" canvas
      lift $ setStyleAttr "height" "" canvas
      let ofs = (width - height) / -2.0
      lift $ setStyleAttr "top" (show ofs ++ "px") canvas
      lift $ setStyleAttr "bottom" (show ofs ++ "px") canvas

      lift $ classAdd "fullWindow" cont
      lift $ classRemove "hide" menu
      return unit


-- hides malformed html issues
updateLayout :: forall eff h. UIConf -> UIST -> SystemST h -> Pattern -> EpiS eff h Unit
updateLayout uiConf uiST systemST pattern = do
  when (systemST.frameNum `mod` uiConf.uiUpdateFreq == 0) do
    when uiConf.showFps do
      case systemST.fps of
        (Just fps) -> do
          fpsDiv <- findElt uiConf.fpsId
          lift $ setInnerHTML (show fps) fpsDiv
        Nothing -> return unit

    when (uiConf.windowState == "dev") do
      -- debug state
      dsmDiv <- findElt "debugMain"
      str0 <- serializeDebugState systemST.moduleRefPool 0 pattern.main ("<span style='color:pink'>MAIN: " ++ pattern.main ++ "</span>")
      lift $ setInnerHTML str0 dsmDiv

      dsdDiv <- findElt "debugDisp"
      str1 <- serializeDebugState systemST.moduleRefPool 0 pattern.disp ("<span style='color:pink'>DISP: " ++ pattern.disp ++ "</span>")
      lift $ setInnerHTML str1 dsdDiv

      dssDiv <- findElt "debugScripts"
      str2 <- serializeScripts systemST.scriptRefPool
      lift $ setInnerHTML ("<span style='color:pink'>SCRIPTS:</span>\n" ++ str2) dssDiv

    return unit

-- serialize scripts for debug view
serializeScripts :: forall eff h. StrMap (STRef h Script) -> EpiS eff h String
serializeScripts pool = do
  foldM handle "" pool
  where
    handle res k sRef = do
      scr <- lift $ readSTRef sRef
      let title = ("<span style='color:blue;font-weight:bold;'>" ++ k ++ "</span>")
      str <- unsafeSerialize scriptSchema title scr
      return $ str ++ "<br/><br/>" ++ res


-- serializes the modRefPool into an html string for debugging
serializeDebugState :: forall eff h. StrMap (STRef h Module) -> Int -> String -> String -> EpiS eff h String
serializeDebugState pool ofs nl nn = do
  mRef <- loadLib nl pool "serializeDebugState"
  main <- lift $ readSTRef mRef
  str <- unsafeSerialize moduleSchema nn main

  let rgx = regex "modules \\{([^\\{\\}]*)\\}" noFlags
  res <- case (match rgx str) of
    (Just [(Just _), (Just "")]) -> do
      return str
    (Just [(Just m0), (Just m1)]) -> do
      dt <- traverse (exp <<< split ":") $ split "," m1
      let modS = joinWith "\n" dt
      return $ (trim (replace m0 "" $ replace (m0 ++ "\n") "" str)) ++ "\n<span style='color:red'>MODULES</span>\n" ++ modS
    _ ->
      return str

  return $ indentLines ofs res
  where
    exp [a, b] = do
      serializeDebugState pool 2 (trim b) ("<span style='color:blue;font-weight:bold'>" ++ (trim a) ++ ": " ++ (trim b) ++ "</span>")
    exp _ = throwError $ "invalid map syntax in " ++ nl



findElt :: forall eff. String -> Epi eff HTMLElement
findElt id = do
  doc <- lift $ document globalWindow
  elt <- lift $ querySelector ("#" ++ id) doc

  case elt of
    (Just e) -> return e
    Nothing  -> throwError $ "couldn't find element: #" ++ id
