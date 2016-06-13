module Layout where

import Prelude
import Config (EpiS, moduleSchema, Module, Pattern, SystemST, UIST, Epi, UIConf)
import Control.Monad (when, unless)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (readSTRef, STRef)
import Control.Monad.Trans (lift)
import Data.DOM.Simple.Element (setInnerHTML, setStyleAttr, querySelector)
import Data.DOM.Simple.Unsafe.Element (HTMLElement)
import Data.DOM.Simple.Window (innerHeight, innerWidth, document, globalWindow)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (size, StrMap)
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
updateLayout :: forall eff h. UIConf -> UIST -> SystemST h -> Pattern -> EpiS eff h Unit
updateLayout uiConf uiST systemST pattern = do
  when (systemST.frameNum `mod` uiConf.uiUpdateFreq == 0) do
    case systemST.fps of
      (Just fps) -> do
        fpsDiv <- findElt uiConf.fpsId
        lift $ setInnerHTML ((show fps) ++ "fps") fpsDiv
      Nothing -> return unit

    let a = lg uiST.debugState
    when uiST.debugState do
      dsDiv <- findElt uiConf.debugStateId
      str <- renderDebugState systemST.moduleRefPool 0 pattern.main ("<span style='color:pink'>MAIN: " ++ pattern.main ++ "</span>")
      lift $ setInnerHTML str dsDiv
      return unit


renderDebugState :: forall eff h. StrMap (STRef h Module) -> Int -> String -> String -> EpiS eff h String
renderDebugState pool ofs nl nn = do
  mRef <- loadLib nl pool "renderDebugState"
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
      renderDebugState pool 2 (trim b) ("<span style='color:blue'>" ++ (trim a) ++ ": " ++ (trim b) ++ "</span>")
    exp _ = throwError $ "invalid map syntax in " ++ nl



findElt :: forall eff. String -> Epi eff HTMLElement
findElt id = do
  doc <- lift $ document globalWindow
  elt <- lift $ querySelector ("#" ++ id) doc

  case elt of
    (Just e) -> return e
    Nothing  -> throwError $ "couldn't find element: #" ++ id
