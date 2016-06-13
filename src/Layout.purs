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
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(Just, Nothing))
import Data.StrMap (StrMap)
import Data.String (joinWith, trim, split, replace)
import Data.String.Regex (match, noFlags, regex)
import Data.Traversable (traverse)
import Serialize (SerializeError(SerializeError), unsafeSerialize)
import System (loadLib)
import Util (indentLines, lg)

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
      (Just fps) -> showFps uiConf.fpsId fps
      Nothing -> return unit

    when uiST.debugState do
      dsDiv <- findElt uiConf.debugStateId
      str <- renderDebugState systemST.moduleRefPool 0 pattern.main
      lift $ setInnerHTML str dsDiv
      return unit


renderDebugState :: forall eff h. StrMap (STRef h Module) -> Int -> String -> EpiS eff h String
renderDebugState pool ofs n = do
  mRef <- loadLib n pool "renderDebugState"
  main <- lift $ readSTRef mRef
  str <- case unsafeSerialize moduleSchema n main of
    (Left (SerializeError er)) -> throwError $ "Error serializing object " ++ n ++ " : " ++ er
    (Right s) -> return s

  let rgx = regex "modules \\{([^\\{\\}]*)\\}\n" noFlags
  let a = lg $ match rgx str
  res <- case (match rgx str) of
    (Just [(Just _), (Just "")]) -> do
      return str
    (Just [(Just m0), (Just m1)]) -> do
      dt <- traverse (exp <<< split ":") $ split "," m1
      let modS = joinWith "\n" dt
      return $ (replace m0 "" str) ++ "\nMODULES\n" ++ modS
    _ ->
      return str

  return $ indentLines ofs res
  where
    exp [a, b] = do
      mn <- renderDebugState pool 2 (trim b)
      return $ "  ##" ++ (trim a) ++ "\n" ++ mn
    exp _ = throwError $ "invalid map syntax in " ++ n



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
