module KeyHandlers where

import Prelude
import Config (UIST, UIConf)
import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST, STRef, readSTRef, modifySTRef)
import DOM (DOM)
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (insert, member, lookup)
import Graphics.Canvas (Canvas)

-- converts key codes into command sequences
type KeyHandler = forall eff h. STRef h UIConf -> STRef h UIST -> String -> Eff (canvas :: Canvas, dom :: DOM, st :: ST h | eff) String

keyHandler :: KeyHandler
keyHandler ucRef usRef char = do
  uiConf <- readSTRef ucRef

  --let x = lg char
  case uiConf.keySet of
    "dev"  -> devKeyHandler ucRef usRef char
    "prod" -> prodKeyHandler ucRef usRef char
    _      -> commonKeyHandler ucRef usRef char


devKeyHandler :: KeyHandler
devKeyHandler ucRef usRef char = do
  uiConf <- readSTRef ucRef
  uiST   <- readSTRef usRef

  case char of
    "1" -> do
      inc uiConf uiST "Sub" "main.main_body.t" "t_inner" "t_inner" "vec2" 1
    "q" -> do
      inc uiConf uiST "Sub" "main.main_body.t" "t_inner" "t_inner" "vec2" (-1)
    "2" -> do
      inc uiConf uiST "Mod" "disp" "post" "basic" "vec4" 1
    "w" -> do
      inc uiConf uiST "Mod" "disp" "post" "basic" "vec4" (-1)
    "3" -> do
      inc uiConf uiST "Mod" "main.main_body" "color" "basic" "vec4" 1
    "e" -> do
      inc uiConf uiST "Mod" "main.main_body" "color" "basic" "vec4" (-1)
    "4" -> do
      inc uiConf uiST "Mod" "main.main_body" "seed" "basic" "vec4" 1
    "r" -> do
      inc uiConf uiST "Mod" "main.main_body" "seed" "basic" "vec4" (-1)
    "a" -> do
      return "scr incZn main.main_body.t idx:0 ofs:1"
    "z" -> do
      return "scr incZn main.main_body.t idx:0 ofs:-1"
    "A" -> do
      return "scr incZn main.main_body.t idx:0 ofs:i"
    "Z" -> do
      return "scr incZn main.main_body.t idx:0 ofs:-i"
    "s" -> do
      return "scr incZn main.main_body.t idx:1 ofs:1"
    "x" -> do
      return "scr incZn main.main_body.t idx:1 ofs:-1"
    "S" -> do
      return "scr incZn main.main_body.t idx:1 ofs:i"
    "X" -> do
      return "scr incZn main.main_body.t idx:1 ofs:-i"
    "d" -> do
      return "scr incZn main.main_body.t idx:2 ofs:1"
    "c" -> do
      return "scr incZn main.main_body.t idx:2 ofs:-1"
    "D" -> do
      return "scr incZn main.main_body.t idx:2 ofs:i"
    "C" -> do
      return "scr incZn main.main_body.t idx:2 ofs:-i"
    "f" -> do
      return "scr incZn main.main_body.t idx:3 ofs:1"
    "v" -> do
      return "scr incZn main.main_body.t idx:3 ofs:-1"
    "F" -> do
      return "scr incZn main.main_body.t idx:3 ofs:i"
    "V" -> do
      return "scr incZn main.main_body.t idx:3 ofs:-i"
    _   -> commonKeyHandler ucRef usRef char
  where
    inc uiConf uiST typ adr sub lib dim ofs = do
      let idn' = adr ++ sub
      let idx = if (member idn' uiST.incIdx) then ((fromJust $ lookup idn' uiST.incIdx) + ofs) else 0
      let dt = insert idn' idx uiST.incIdx
      modifySTRef usRef (\s -> s {incIdx = dt})
      let spd = show uiConf.keyboardSwitchSpd
      return $ "scr inc" ++ typ ++ " " ++ adr ++ " sub:" ++ sub ++ " lib:" ++ lib ++ " dim:" ++ dim ++ " idx:" ++ (show idx) ++ " spd:" ++ spd

prodKeyHandler :: KeyHandler
prodKeyHandler ucRef usRef char = do
  uiConf <- readSTRef ucRef
  uiST   <- readSTRef usRef

  case char of
    _ -> commonKeyHandler ucRef usRef char

commonKeyHandler :: KeyHandler
commonKeyHandler ucRef usRef char = do
  uiConf <- readSTRef ucRef
  uiST   <- readSTRef usRef

  case char of
    "~"  -> return "dev"
    "|"  -> return "showFps"
    "\\" -> return "clear"
    " "  -> return "save"
    _    -> return "null"
