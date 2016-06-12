module UI where

import Prelude

import Data.Maybe (Maybe (Just))
import Data.Int
import Data.StrMap (insert, member, lookup)
import Data.Maybe.Unsafe (fromJust)

import Control.Monad (when, unless)
import Control.Monad.Eff (Eff)
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
import Util (lg)

foreign import registerEventHandler :: forall eff. (String -> Eff eff Unit) -> Eff eff Unit
foreign import registerKeyHandler :: forall eff. (String -> Eff eff String) -> Eff eff Unit
foreign import requestFullScreen :: forall eff. String -> Eff eff Unit

-- PUBLIC
initUIST :: forall eff h. STRef h UIConf -> STRef h EngineConf -> STRef h EngineST -> STRef h Pattern -> STRef h SystemConf -> STRef h (SystemST h) -> EpiS eff h (STRef h UIST)
initUIST ucRef ecRef esRef pRef scRef ssRef = do
  uiConf <- lift $ readSTRef ucRef
  let uiST = defaultUIST
  usRef <- lift $ newSTRef uiST

  initLayout uiConf
  lift $ registerEventHandler (command ucRef usRef ecRef esRef pRef scRef ssRef)
  lift $ registerKeyHandler (keyHandler ucRef usRef)

  return usRef


keyHandler :: forall eff h. STRef h UIConf -> STRef h UIST -> String -> Eff (canvas :: Canvas, dom :: DOM, st :: ST h | eff) String
keyHandler ucRef usRef char = do
  uiConf <- readSTRef ucRef
  uiST   <- readSTRef usRef

  let x = lg char
  let spd = show uiConf.keyboardSwitchSpd
  case char of
    "1" -> do
      incM uiST "main.main_body" "t" "basic_t" "vec2" 1 spd
    "Q" -> do
      incM uiST "main.main_body" "t" "basic_t" "vec2" (-1) spd
    "2" -> do
      incI uiST "main.main_body.t" "t_inner" "t_inner" "vec2" 1 spd
    "W" -> do
      incI uiST "main.main_body.t" "t_inner" "t_inner" "vec2" (-1) spd
    "3" -> do
      incM uiST "main.main_body.seed.seed0" "t" "basic_seed" "vec2" 1 spd
    "E" -> do
      incM uiST "main.main_body.seed.seed0" "t" "basic_seed" "vec2" (-1) spd
    "4" -> do
      incI uiST "main.main_body.seed.seed0.t" "t_inner" "t_inner" "vec2" 1 spd
    "R" -> do
      incI uiST "main.main_body.seed.seed0.t" "t_inner" "t_inner" "vec2" (-1) spd
    "5" -> do
      incM uiST "main.main_body" "color" "basic" "vec4" 1 spd
    "T" -> do
      incM uiST "main.main_body" "color" "basic" "vec4" (-1) spd
    "6" -> do
      incM uiST "disp" "post" "basic" "vec4" 1 spd
    "Y" -> do
      incM uiST "disp" "post" "basic" "vec4" (-1) spd
    "0" -> do
      incM uiST "main.main_body.seed.seed0" "seed_color" "basic" "vec4" 1 spd
    "P" -> do
      incM uiST "main.main_body.seed.seed0" "seed_color" "basic" "vec4" (-1) spd
    "½" -> do
      incM uiST "main.main_body.seed.seed1" "seed_color" "basic" "vec4" 1 spd
    "Û" -> do
      incM uiST "main.main_body.seed.seed1" "seed_color" "basic" "vec4" (-1) spd
    "Ü" -> return $ "clear"
    " " -> return $ "save"
    _   -> return $ "null"
  where
    incM uiST bdy idn lib dim inc spd = do
      let idn' = bdy ++ idn
      let idx = if (member idn' uiST.incIdx) then ((fromJust $ lookup idn' uiST.incIdx) + inc) else 0
      let dt = insert idn' idx uiST.incIdx
      modifySTRef usRef (\s -> s {incIdx = dt})
      return $ "scr incMod " ++  bdy ++ " sub:" ++ idn ++ " lib:" ++ lib ++ " dim:" ++ dim ++ " idx:" ++ (show idx) ++ " spd:" ++ spd
    incI uiST addr sub lib dim inc spd = do
      let idn' = addr ++ sub
      let idx = if (member idn' uiST.incIdx) then ((fromJust $ lookup idn' uiST.incIdx) + inc) else 0
      let dt = insert idn' idx uiST.incIdx
      modifySTRef usRef (\s -> s {incIdx = dt})
      return $ "scr incSub " ++  addr ++ " sub:" ++ sub ++ " lib:" ++ lib ++ " dim:" ++ dim ++ " idx:" ++ (show idx) ++ " spd:" ++ spd


initLayout :: forall eff. UIConf -> Epi eff Unit
initLayout uiConf = do
  let window = globalWindow
  doc <- lift $ document window
  width  <- lift $ innerWidth window
  height <- lift $ innerHeight window

  unless uiConf.fullScreen do
    -- this is unsafe
    Just c2 <- lift $ querySelector ("#" ++ uiConf.canvasId) doc
    lift $ setStyleAttr "width" (show (height - 10.0) ++ "px") c2
    lift $ setStyleAttr "height" (show (height - 11.0) ++ "px") c2

    Just console <- lift $ querySelector ("#" ++ uiConf.consoleId) doc
    lift $ setStyleAttr "width" (show (width - height - 30.0) ++ "px") console
    lift $ setStyleAttr "height" (show (height - 21.0) ++ "px") console

  when uiConf.fullScreen do
    --lift $ requestFullScreen uiConf.canvasId
    -- this is unsafe
    Just c2 <- lift $ querySelector ("#" ++ uiConf.canvasId) doc
    lift $ setStyleAttr "width" "100%" c2
    lift $ setStyleAttr "height" "100%" c2

-- hackish
showFps :: forall eff. Int -> Epi eff Unit
showFps fps = do
  let window = globalWindow
  doc <- lift $ document window
  Just fpsDiv <- lift $ querySelector ("#showfps") doc
  lift $ setInnerHTML ((show fps) ++ "fps") fpsDiv
