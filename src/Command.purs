module Command where

import Prelude

import Data.Array (length, head, tail, foldM, (!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (StrMap(..), insert)
import Data.String (split)
import Data.Tuple (Tuple(..))

import Control.Monad (unless)
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (runExceptT, lift)
import Control.Monad.ST

import Graphics.Canvas (Canvas)
import DOM (DOM)

import Config
import System (loadLib)
import Pattern (importScript, findModule)
import Util (winLog, lg, handleError)
import Engine (clearFB)

command :: forall eff h. STRef h UIConf -> STRef h UIST -> STRef h EngineConf -> STRef h EngineST -> STRef h Pattern -> STRef h SystemConf -> STRef h (SystemST h) -> String -> Eff (canvas :: Canvas, dom :: DOM, st :: ST h | eff) Unit
command ucRef usRef ecRef esRef pRef scRef ssRef msg = handleError do
  systemST   <- lift $ readSTRef ssRef
  uiConf     <- lift $ readSTRef ucRef
  usConf     <- lift $ readSTRef usRef
  engineConf <- lift $ readSTRef ecRef
  engineST   <- lift $ readSTRef esRef
  pattern    <- lift $ readSTRef pRef

  let x = lg (show msg)

  let dt = split " " msg

  unless (length dt == 0) do
    let cmd = fromJust $ head dt
    let args = fromJust $ tail dt

    case cmd of
      "null" -> return unit
      "pause" -> do
        lift $ modifySTRef pRef (\p -> p {tSpd = 1.0 - p.tSpd})
        return unit
      "scr" -> do
        -- build
        scr <- loadLib "default" systemST.scriptLib "building script"
        Tuple scr' _ <- foldM (parseScript systemST.moduleRefPool pattern) (Tuple scr ScrFn) args

        -- import
        mid <- return $ fromJust scr'.mid
        importScript ssRef (Left scr') mid

        return unit
      "clear" -> do
        clearFB engineConf engineST
      _ -> throwError $ "Unknown command: " ++ msg



-- PRIVATE
data ScrPS = ScrFn | ScrMid | ScrDt

-- recursively parse a script from a string
parseScript :: forall eff h. StrMap (STRef h Module) -> Pattern -> (Tuple Script ScrPS) -> String -> EpiS eff h (Tuple Script ScrPS)
parseScript mpool pattern (Tuple scr ps) dt = do
  case ps of
    ScrFn -> do
      return $ Tuple scr {fn = dt} ScrMid
    ScrMid -> do
      mid <- findModule mpool pattern dt
      return $ Tuple scr {mid = Just mid} ScrDt
    ScrDt -> do
      let tok = split ":" dt
      case (length tok) of
        2 -> do
          let dt' = insert (fromJust $ tok !! 0) (fromJust $ tok !! 1) scr.dt
          let scr' = scr {dt = dt'}
          return $ Tuple scr' ScrDt
        _ -> throwError $ "invalid script data assignment :" ++ dt
    _ -> throwError "parseScript - wtf?"
