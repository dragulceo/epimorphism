module UI where

import Prelude
import Control.Monad.ST (ST, STRef, readSTRef, modifySTRef, newSTRef)
import Graphics.Canvas (Canvas)
import Config (UIST, UIConf, EpiS, SystemST, SystemConf, Pattern, EngineST, EngineConf, defaultUIST)
import Command (command)
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (lift)
import DOM (DOM)
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (insert, member, lookup)
import Layout(initLayout)
--import Util (lg)

foreign import registerEventHandler :: forall eff. (String -> Eff eff Unit) -> Eff eff Unit
foreign import registerKeyHandler :: forall eff. (String -> Eff eff String) -> Eff eff Unit
foreign import requestFullScreen :: forall eff. String -> Eff eff Unit

-- PUBLIC
initUIST :: forall eff h. STRef h UIConf -> STRef h EngineConf -> STRef h EngineST -> STRef h Pattern -> STRef h SystemConf -> STRef h (SystemST h) -> EpiS eff h (STRef h UIST)
initUIST ucRef ecRef esRef pRef scRef ssRef = do
  uiConf <- lift $ readSTRef ucRef
  let uiST = defaultUIST
  usRef <- lift $ newSTRef uiST

  initLayout uiConf uiST
  lift $ registerEventHandler (command ucRef usRef ecRef esRef pRef scRef ssRef)
  lift $ registerKeyHandler (keyHandler ucRef usRef)

  return usRef


keyHandler :: forall eff h. STRef h UIConf -> STRef h UIST -> String -> Eff (canvas :: Canvas, dom :: DOM, st :: ST h | eff) String
keyHandler ucRef usRef char = do
  uiConf <- readSTRef ucRef
  uiST   <- readSTRef usRef

  --let x = lg char
  let spd = show uiConf.keyboardSwitchSpd
  case char of
    "1" -> do
      incM uiST "main.main_body" "seed" "fam1" "vec4" 1 spd
    "Q" -> do
      incM uiST "main.main_body" "seed" "fam1" "vec4" (-1) spd
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
