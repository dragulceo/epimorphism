module Command where

import Prelude
import Config (scriptSchema, Schema, patternSchema, moduleSchema, Script, EpiS, Pattern, Module, SystemST, SystemConf, EngineST, EngineConf, UIST, UIConf)
import Control.Monad (unless)
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (STRef, ST, modifySTRef, readSTRef)
import DOM (DOM)
import Data.Array (length, head, tail, foldM, (!!))
import Data.Int (fromString)
import Data.List (fromList)
import Data.Maybe (Maybe(Just))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (toList, StrMap, insert)
import Data.String (joinWith, split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Engine (initEngineST, clearFB)
import Graphics.Canvas (Canvas)
import Layout (initLayout)
import Pattern (ImportObj(ImportScript), importScript, findModule)
import Serialize (unsafeSerialize)
import System (loadLib)
import Util (uuid, lg, handleError)

command :: forall eff h. STRef h UIConf -> STRef h UIST -> STRef h EngineConf -> STRef h EngineST -> STRef h Pattern -> STRef h SystemConf -> STRef h (SystemST h) -> String -> Eff (canvas :: Canvas, dom :: DOM, st :: ST h | eff) Unit
command ucRef usRef ecRef esRef pRef scRef ssRef msg = handleError do
  systemConf <- lift $ readSTRef scRef
  systemST   <- lift $ readSTRef ssRef
  uiConf     <- lift $ readSTRef ucRef
  uiST       <- lift $ readSTRef usRef
  engineConf <- lift $ readSTRef ecRef
  engineST   <- lift $ readSTRef esRef
  pattern    <- lift $ readSTRef pRef

  let x = lg $ "EXECUTE: " ++ (show msg)

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
        mid <- return $ scr'.mid
        importScript ssRef (ImportScript scr') mid

        return unit
      "save" -> do
        save systemST pattern
      "fullWindow" -> do
        lift $ modifySTRef ucRef (\ui -> ui {windowState = "full"})
        uiConf' <- lift $ readSTRef ucRef
        initLayout uiConf' uiST

        return unit
      "dev" -> do
        lift $ modifySTRef ucRef (\ui -> ui {windowState = "dev", keySet = "dev"})
        uiConf' <- lift $ readSTRef ucRef
        initLayout uiConf' uiST

        return unit
      "initLayout" -> do
        initLayout uiConf uiST
        return unit
      "showFps" -> do
        lift $ modifySTRef ucRef (\ui -> ui {showFps = not ui.showFps})
        uiConf' <- lift $ readSTRef ucRef
        initLayout uiConf' uiST

        return unit
      "setKernelDim" -> do
        case args of
          [dim] -> do
            case (fromString dim) of
              Just dim' -> do
                lift $ modifySTRef ecRef (\ec -> ec {kernelDim = dim'})
                engineConf' <- lift $ readSTRef ecRef
                initEngineST systemConf engineConf' systemST pattern uiConf.canvasId (Just esRef)

                return unit
              _ -> return unit -- should probably do some error handling here
          _ -> return unit -- should probably do some error handling here
        return unit
      "clear" -> do
        clearFB engineConf engineST
      _ -> throwError $ "Unknown command: " ++ msg


-- PRIVATE
save :: forall eff h. (SystemST h) -> Pattern -> EpiS eff h Unit
save systemST pattern = do
  -- pattern
  id <- lift $ uuid
  ps <- unsafeSerialize patternSchema id pattern

  -- modules
  mods <- (traverse (serializeTup moduleSchema) $ fromList $ toList systemST.moduleRefPool) :: EpiS eff h (Array String)
  let mres = joinWith "\n\n" mods

  -- scripts
  scrs <- (traverse (serializeTup scriptSchema) $ fromList $ toList systemST.scriptRefPool) :: EpiS eff h (Array String)
  let sres = joinWith "\n\n" scrs

  let res = "#PATTERN\n" ++ ps ++ "\n\n#MODULES\n" ++ mres ++ "\n\n#SCRIPTS\n" ++ sres
  let a = lg res

  return unit

  where
    serializeTup :: forall a. Schema -> (Tuple String (STRef h a)) -> EpiS eff h String
    serializeTup schema (Tuple n ref) = do
      obj <- lift $ readSTRef ref
      st <- unsafeSerialize schema n obj
      return st


-- recursively parse a script from a string
data ScrPS = ScrFn | ScrMid | ScrDt

parseScript :: forall eff h. StrMap (STRef h Module) -> Pattern -> (Tuple Script ScrPS) -> String -> EpiS eff h (Tuple Script ScrPS)
parseScript mpool pattern (Tuple scr ps) dt = do
  case ps of
    ScrFn -> do
      return $ Tuple scr {fn = dt} ScrMid
    ScrMid -> do
      mid <- findModule mpool pattern dt true
      return $ Tuple scr {mid = mid} ScrDt
    ScrDt -> do
      let tok = split ":" dt
      case (length tok) of
        2 -> do
          let dt' = insert (fromJust $ tok !! 0) (fromJust $ tok !! 1) scr.dt
          let scr' = scr {dt = dt'}
          return $ Tuple scr' ScrDt
        _ -> throwError $ "invalid script data assignment :" ++ dt
