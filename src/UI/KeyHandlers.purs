module KeyHandlers where

import Prelude
import Config (UIST, UIConf)
import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST, STRef, readSTRef, modifySTRef)
import DOM (DOM)
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (insert, member, lookup)
import Graphics.Canvas (Canvas)
import Util (lg)

-- converts key codes into command sequences
type KeyHandler = forall eff h. STRef h UIConf -> STRef h UIST -> String -> Eff (canvas :: Canvas, dom :: DOM, st :: ST h | eff) String

keyHandler :: KeyHandler
keyHandler ucRef usRef char = do
  uiConf <- readSTRef ucRef

  case uiConf.keySet of
    "dev"  -> devKeyHandler ucRef usRef char
    "prod" -> prodKeyHandler ucRef usRef char
    _      -> commonKeyHandler ucRef usRef char


devKeyHandler :: KeyHandler
devKeyHandler ucRef usRef char = do
  uiConf <- readSTRef ucRef
  uiST   <- readSTRef usRef

  --let x = lg char
  let spd = show uiConf.keyboardSwitchSpd
  case char of
    "1" -> do
      incI uiST "main.main_body.t" "t_inner" "t_inner" "vec2" 1 spd
    "Q" -> do
      incI uiST "main.main_body.t" "t_inner" "t_inner" "vec2" (-1) spd
    "2" -> do
      incM uiST "disp" "post" "post_hlim" "vec4" 1 spd
    "W" -> do
      incM uiST "disp" "post" "post_hlim" "vec4" (-1) spd
    "3" -> do
      incG uiST "main.main_body.seed" "0" "basic_images" "vec4" 1 spd
    "E" -> do
      incG uiST "main.main_body.seed" "0" "basic_images" "vec4" (-1) spd
--    "3" -> do
--      incM uiST "main.main_body.seed.seed0" "t" "basic_seed" "vec2" 1 spd
--    "E" -> do
--      incM uiST "main.main_body.seed.seed0" "t" "basic_seed" "vec2" (-1) spd
--    "4" -> do
--      incI uiST "main.main_body.seed.seed0.t" "t_inner" "t_inner" "vec2" 1 spd
--    "R" -> do
--      incI uiST "main.main_body.seed.seed0.t" "t_inner" "t_inner" "vec2" (-1) spd
--    "5" -> do
--      incM uiST "main.main_body" "color" "basic" "vec4" 1 spd
--    "T" -> do
--      incM uiST "main.main_body" "color" "basic" "vec4" (-1) spd
--    "0" -> do
--      incM uiST "main.main_body.seed.seed0" "seed_color" "basic" "vec4" 1 spd
--    "P" -> do
--      incM uiST "main.main_body.seed.seed0" "seed_color" "basic" "vec4" (-1) spd
--    "½" -> do
--      incM uiST "main.main_body.seed.seed1" "seed_color" "basic" "vec4" 1 spd
--    "Û" -> do
--      incM uiST "main.main_body.seed.seed1" "seed_color" "basic" "vec4" (-1) spd
    "6" -> do
      incS uiST "main.main_body.t" "0" "z1" "vec2" 1 spd
    "Y" -> do
      incS uiST "main.main_body.t" "0" "z1" "vec2" (-1) spd
    _   -> commonKeyHandler ucRef usRef char
  where
    incG uiST bdy idn lib dim inc spd = do
      let idn' = bdy ++ idn
      let idx = if (member idn' uiST.incIdx) then ((fromJust $ lookup idn' uiST.incIdx) + inc) else 0
      let dt = insert idn' idx uiST.incIdx
      modifySTRef usRef (\s -> s {incIdx = dt})
      return $ "scr incImage " ++  bdy ++ " sub:" ++ idn ++ " lib:" ++ lib ++ " dim:" ++ dim ++ " idx:" ++ (show idx) ++ " spd:" ++ spd
    incS uiST bdy idn lib dim inc spd = do
      let idn' = bdy ++ idn
      let idx = if (member idn' uiST.incIdx) then ((fromJust $ lookup idn' uiST.incIdx) + inc) else 0
      let dt = insert idn' idx uiST.incIdx
      modifySTRef usRef (\s -> s {incIdx = dt})
      return $ "scr incScript " ++  bdy ++ " sub:" ++ idn ++ " lib:" ++ lib ++ " dim:" ++ dim ++ " idx:" ++ (show idx) ++ " spd:" ++ spd
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
