module KeyHandlers where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.ST (ST, STRef, modifySTRef, readSTRef)
import DOM (DOM)
import Data.Either (either)
import Data.Library (getUIConfD)
import Data.Maybe (Maybe(..))
import Data.StrMap (insert, lookup)
import Data.Types (EpiS, Library, UIST)
import Graphics.Canvas (CANVAS)
import Util (inj)

-- converts key codes into command sequences
type KeyHandler = forall eff h. STRef h UIST -> Library h -> String -> Eff (canvas :: CANVAS, dom :: DOM, st :: ST h | eff) String

type EpiSKeyHandler = forall eff h. STRef h UIST -> Library h -> String -> EpiS eff h String

keyHandler :: KeyHandler
keyHandler usRef lib char = do
  --let x = lg char
  res <- runExceptT do
    uiConfD <- getUIConfD lib "keyHandler"
    case uiConfD.keySet of
      "dev"  -> devKeyHandler usRef lib char
      "prod" -> prodKeyHandler usRef lib char
      _      -> commonKeyHandler usRef lib char

  pure $ either (\_ -> "") id res


devKeyHandler :: EpiSKeyHandler
devKeyHandler usRef lib char = do
  uiConfD <- getUIConfD lib "devKeyHandler"
  uiST    <- liftEff $ readSTRef usRef

  case char of
    "1" -> do
      incMod uiConfD uiST "main.application.t" "t_inner" "all" 1
    "q" -> do
      incMod uiConfD uiST "main.application.t" "t_inner" "all" (-1)
    "!" -> do
      incMod uiConfD uiST "main.application" "t" "all''" 1
    "Q" -> do
      incMod uiConfD uiST "main.application" "t" "all''" (-1)
    "1" -> do
      incMod uiConfD uiST "main.application.t" "t_inner" "all" 1
    "q" -> do
      incMod uiConfD uiST "main.application.t" "t_inner" "all" (-1)
    "2" -> do
      incMod uiConfD uiST "main.application" "seed" "t_test" 1
    "w" -> do
      incMod uiConfD uiST "main.application" "seed" "t_test" (-1)
    "3" -> do
      incMod uiConfD uiST "main.application" "color" "lib" 1
    "e" -> do
      incMod uiConfD uiST "main.application" "color" "lib" (-1)
    "4" -> do
      incMod uiConfD uiST "disp" "post" "lib" 1
    "r" -> do
      incMod uiConfD uiST "disp" "post" "lib" (-1)
    "a" -> do
      pure "scr main.application.t incZn idx:0 ofs:1"
    "z" -> do
      pure "scr main.application.t incZn idx:0 ofs:-1"
    "A" -> do
      pure "scr main.application.t incZn idx:0 ofs:i"
    "Z" -> do
      pure "scr main.application.t incZn idx:0 ofs:-i"
    "s" -> do
      pure "scr main.application.t incZn idx:1 ofs:1"
    "x" -> do
      pure "scr main.application.t incZn idx:1 ofs:-1"
    "S" -> do
      pure "scr main.application.t incZn idx:1 ofs:i"
    "X" -> do
      pure "scr main.application.t incZn idx:1 ofs:-i"
    "d" -> do
      pure "scr main.application.t incZn idx:2 ofs:1"
    "c" -> do
      pure "scr main.application.t incZn idx:2 ofs:-1"
    "D" -> do
      pure "scr main.application.t incZn idx:2 ofs:i"
    "C" -> do
      pure "scr main.application.t incZn idx:2 ofs:-i"
    "f" -> do
      pure "scr main.application.t incZn idx:3 ofs:1"
    "v" -> do
      pure "scr main.application.t incZn idx:3 ofs:-1"
    "F" -> do
      pure "scr main.application.t incZn idx:3 ofs:i"
    "V" -> do
      pure "scr main.application.t incZn idx:3 ofs:-i"
    _   -> commonKeyHandler usRef lib char
  where
    incMod uiConfD uiST adr childN lib' ofs = do
      let idn' = adr <> childN
      idx <- case lookup idn' uiST.incIdx of
        Just i -> pure $ i + ofs
        Nothing -> pure $ ofs
      let dt = insert idn' idx uiST.incIdx
      liftEff $ modifySTRef usRef (\s -> s {incIdx = dt})
      let spd = show uiConfD.keyboardSwitchSpd
      pure $ inj "scr %0 switch childN:%1 op:load by:query typ:mod query:%2 accs:%3 spd:%4" [adr, childN, lib', (show idx), spd]



prodKeyHandler :: EpiSKeyHandler
prodKeyHandler usRef lib char = do
  case char of
    _ -> commonKeyHandler usRef lib char

commonKeyHandler :: EpiSKeyHandler
commonKeyHandler usRef lib char = do
  case char of
    "~"  -> pure "dev"
    "|"  -> pure "showFps"
    "\\" -> pure "clear"
    " "  -> pure "save"
    "`"  -> pure "pause"
    "?"  -> pure "halt"
    _    -> pure "null"
