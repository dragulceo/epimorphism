module Command where

import Prelude
import Config (Schema, patternSchema, moduleSchema, EpiS, Pattern, SystemST, SystemConf, EngineST, EngineConf, UIST, UIConf)
import Control.Monad (when, unless)
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (writeSTRef, STRef, ST, modifySTRef, readSTRef)
import DOM (DOM)
import Data.Array (uncons, updateAt, length, head, tail)
import Data.List (fromList)
import Data.Maybe (Maybe(Just))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (values, insert, toList)
import Data.String (joinWith, split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Texture (clearFB)
import Engine (setShaders, initEngineST)
import Graphics.Canvas (Canvas)
import Layout (updateLayout, initLayout)
import Pattern (findModule)
import ScriptUtil (addScript)
import Serialize (unsafeSerialize)
import System (mUp, loadLib)
import Util (halt, Now, cxFromStringE, intFromStringE, numFromStringE, lg, uuid, handleError)

foreign import saveCanvas :: forall eff. Eff eff Unit

command :: forall eff h. STRef h UIConf -> STRef h UIST -> STRef h EngineConf -> STRef h EngineST -> STRef h Pattern -> STRef h SystemConf -> STRef h (SystemST h) -> String -> Eff (canvas :: Canvas, dom :: DOM, st :: ST h, now :: Now | eff) Unit
command ucRef usRef ecRef esRef pRef scRef ssRef msg = handleError do
  systemConf <- lift $ readSTRef scRef
  systemST   <- lift $ readSTRef ssRef
  uiConf     <- lift $ readSTRef ucRef
  uiST       <- lift $ readSTRef usRef
  engineConf <- lift $ readSTRef ecRef
  engineST   <- lift $ readSTRef esRef
  pattern    <- lift $ readSTRef pRef

  --let x = lg $ "EXECUTE: " ++ (show msg)

  let dt = split " " msg

  unless (length dt == 0) do
    let cmd = fromJust $ head dt
    let args = fromJust $ tail dt

    case cmd of
      "null" -> return unit
      "pause" -> do
        lift $ modifySTRef ssRef (\s -> s {paused = not s.paused})
        return unit
      "halt" -> do
        lift $ halt
        return unit
      "pauseAfterSwitch" -> do
        lift $ modifySTRef ssRef (\s -> s {pauseAfterSwitch = true})
        return unit
      "killScripts" -> do
        let upd = (\r -> lift $ modifySTRef r (\m -> m {scripts = []}))
        traverse upd (values systemST.moduleRefPool)

        return unit
      "scr" -> do
        when (length args >= 2) do -- check for errors here
          {head: addr, tail: rst}  <- return $ fromJust $ uncons args
          {head: name, tail: rst'} <- return $ fromJust $ uncons rst
          mid <- findModule systemST.moduleRefPool pattern addr true
          addScript systemST mid name (joinWith " " rst')

          return unit
      "setP" -> do
        case args of
          [addr, par, val] -> do
            val' <- numFromStringE val
            mid  <- findModule systemST.moduleRefPool pattern addr true
            mUp systemST mid \m ->
              m {par = insert par (show val') m.par}

            return unit
          _ -> throwError "invalid format: setP addr par val"
      "setZn" -> do
        case args of
          [addr, idx, val] -> do
            val' <- cxFromStringE val
            idx' <- intFromStringE idx
            mid  <- findModule systemST.moduleRefPool pattern addr true
            mRef <- loadLib mid systemST.moduleRefPool "find module - setP"
            mod <- lift $ readSTRef mRef

            zn' <- case (updateAt idx' (show val') mod.zn) of
              Just x -> return x
              _ -> throwError "zn idx out of bounds setZn"
            lift $ modifySTRef mRef (\m -> m {zn = zn'})

            return unit
          _ -> throwError "invalid format: setPar addr par val"
      "setT" -> do
        let tExp = joinWith "" args
        mid <- findModule systemST.moduleRefPool pattern "main.application.t.t_inner" true
        mUp systemST mid \m ->
          m {sub = insert "t_expr" tExp m.sub}
        setShaders systemConf engineConf esRef systemST pattern

        return unit
      "save" -> do
        save systemST pattern
      "fullWindow" -> do
        lift $ modifySTRef ucRef (\ui -> ui {windowState = "full"})
        uiConf' <- lift $ readSTRef ucRef
        initLayout uiConf' uiST

        return unit
      "dev" -> do
        lift $ modifySTRef ucRef (\ui -> ui {windowState = "dev", keySet = "dev", showFps = true})
        uiConf' <- lift $ readSTRef ucRef
        initLayout uiConf' uiST

        return unit
      "initLayout" -> do
        initLayout uiConf uiST
        return unit
      "updateLayout" -> do
        updateLayout uiConf uiST systemST pattern true
      "showFps" -> do
        lift $ modifySTRef ucRef (\ui -> ui {showFps = not ui.showFps})
        uiConf' <- lift $ readSTRef ucRef
        initLayout uiConf' uiST

        return unit
      "setKernelDim" -> do
        case args of
          [dim] -> do
            dim' <- intFromStringE dim
            lift $ modifySTRef ecRef (\ec -> ec {kernelDim = dim'})
            engineConf' <- lift $ readSTRef ecRef
            initEngineST systemConf engineConf' systemST pattern uiConf.canvasId (Just esRef)

          _ -> throwError "invalid format: setKerneldim dim"
        return unit
      "setFract" -> do
        case args of
          [fract] -> do
            fract' <- intFromStringE fract
            lift $ modifySTRef ecRef (\ec -> ec {fract = fract'})
            engineConf' <- lift $ readSTRef ecRef
            setShaders systemConf engineConf' esRef systemST pattern

          _ -> throwError "invalid format: setFract fract"
        return unit
      "setEngineProfile" -> do
        case args of
          [lib] -> do
            engineConf' <- loadLib lib systemST.engineConfLib "setEngineProfile"
            lift $ writeSTRef ecRef engineConf'

            initEngineST systemConf engineConf' systemST pattern uiConf.canvasId (Just esRef)

          _ -> throwError "invalid format: setFract fract"
        return unit
      "clear" -> do
        clearFB engineConf engineST
      _ -> throwError $ "Unknown command: " ++ msg


-- PRIVATE
save :: forall eff h. (SystemST h) -> Pattern -> EpiS eff h Unit
save systemST pattern = do
  -- pattern
  id <- lift $ uuid
  ps <- unsafeSerialize patternSchema (Just id) pattern

  -- modules
  mods <- (traverse (serializeTup moduleSchema) $ fromList $ toList systemST.moduleRefPool) :: EpiS eff h (Array String)
  let mres = joinWith "\n\n" mods

  let res = "#PATTERN\n" ++ ps ++ "\n\n#MODULES\n" ++ mres
  let a = lg res

  lift $ saveCanvas

  return unit

  where
    serializeTup :: forall a. Schema -> (Tuple String (STRef h a)) -> EpiS eff h String
    serializeTup schema (Tuple n ref) = do
      obj <- lift $ readSTRef ref
      st <- unsafeSerialize schema (Just n) obj
      return st
