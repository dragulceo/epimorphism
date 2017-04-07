module Command where

import Prelude
import Compiler (compileShaders)
import Config (SystemST, EngineST, UIST)
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (STRef, ST, modifySTRef, readSTRef)
import DOM (DOM)
import Data.Array (uncons, updateAt, length)
import Data.Library (Library, getEngineConf, getLibM, getPatternD, getSystemConf, getUIConf, getUIConfD, modLibD)
import Data.Maybe (Maybe(..))
import Data.StrMap (values, insert)
import Data.String (joinWith, split)
import Data.String (Pattern(..)) as S
import Data.Traversable (traverse)
import Data.Types (EngineConf, EpiS, PatternD)
import Engine (initEngineST)
import Graphics.Canvas (CANVAS)
import Layout (updateLayout, initLayout)
import Pattern (findModule)
import ScriptUtil (addScript)
import System (mUp, loadLib)
import Texture (clearFB)
import Util (dbg, halt, Now, cxFromStringE, intFromStringE, numFromStringE, lg, uuid, handleError)

foreign import saveCanvas :: forall eff. Eff eff Unit

command :: forall eff h. STRef h UIST -> STRef h EngineST -> STRef h (SystemST h) -> Library h -> String -> Eff (canvas :: CANVAS, dom :: DOM, st :: ST h, now :: Now | eff) Unit
command usRef esRef ssRef lib msg = handleError do
  uiConfD    <- getUIConfD lib "command uiConf"
  patternD   <- getPatternD lib "command pattern"

  systemST   <- lift $ readSTRef ssRef
  uiST       <- lift $ readSTRef usRef
  engineST   <- lift $ readSTRef esRef

  --let x = lg $ "EXECUTE: " <> (show msg)

  let dt = split (S.Pattern " ") msg

  unless (length dt == 0) do
    case uncons dt of
      Nothing -> throwError $ "invalid message " <> msg
      Just {head: cmd, tail: args} -> do
        case cmd of
          "null" -> pure unit
          "pause" -> do
            dbg "pause"
            lift $ modifySTRef ssRef (\s -> s {paused = not s.paused})
            pure unit
          "halt" -> do
            lift $ halt
            pure unit
          "pauseAfterSwitch" -> do
            lift $ modifySTRef ssRef (\s -> s {pauseAfterSwitch = true})
            pure unit
          "killScripts" -> do
            let upd = (\r -> lift $ modifySTRef r (\m -> m {scripts = []}))
            traverse upd (values systemST.moduleRefPool)

            pure unit
          "scr" -> do
            case uncons args of
              Nothing -> throwError $ "invalid script args " <> msg
              Just {head: addr, tail: rst} -> do
                case uncons rst of
                  Nothing -> throwError $ "invalid script args " <> msg
                  Just {head: name, tail: rst'} -> do
                    mid <- findModule systemST.moduleRefPool patternD addr true
                    addScript systemST mid name (joinWith " " rst')
                    pure unit
          "setP" -> do
            case args of
              [addr, par, val] -> do
                val' <- numFromStringE val
                mid  <- findModule systemST.moduleRefPool patternD addr true
                mUp systemST mid \m ->
                  m {par = insert par (show val') m.par}

                pure unit
              _ -> throwError "invalid format: setP addr par val"
          "setZn" -> do
            case args of
              [addr, idx, val] -> do
                val' <- cxFromStringE val
                idx' <- intFromStringE idx
                mid  <- findModule systemST.moduleRefPool patternD addr true
                mRef <- loadLib mid systemST.moduleRefPool "find module - setP"
                mod <- lift $ readSTRef mRef

                zn' <- case (updateAt idx' (show val') mod.zn) of
                  Just x -> pure x
                  _ -> throwError "zn idx out of bounds setZn"
                lift $ modifySTRef mRef (\m -> m {zn = zn'})

                pure unit
              _ -> throwError "invalid format: setPar addr par val"
          "setT" -> do
            let tExp = joinWith "" args
            mid <- findModule systemST.moduleRefPool patternD "main.application.t.t_inner" true
            mUp systemST mid \m ->
              m {sub = insert "t_expr" tExp m.sub}
            compileShaders ssRef esRef lib false

            pure unit
          "save" -> do
            save systemST patternD
          "fullWindow" -> do
            uiConf <- getUIConf lib "fullWindow"
            modLibD lib uiConf _ {windowState = "full"}
            initLayout uiST lib

            pure unit
          "dev" -> do
            uiConf <- getUIConf lib "dev"
            modLibD lib uiConf _ {windowState = "dev", keySet = "dev", showFps = true}
            initLayout uiST lib

            pure unit
          "initLayout" -> do
            initLayout uiST lib
            pure unit
          "updateLayout" -> do
            updateLayout uiST systemST lib true
          "showFps" -> do
            uiConf <- getUIConf lib "showFps"
            modLibD lib uiConf $ \x -> x {showFps = not x.showFps}
            initLayout uiST lib

            pure unit
          "setKernelDim" -> do
            case args of
              [dim] -> do
                dim' <- intFromStringE dim
                engineConf <- getEngineConf lib "setKernelDim engineConf"
                modLibD lib engineConf _ {kernelDim = dim'}
                initEngineST systemST lib uiConfD.canvasId (Just esRef)

              _ -> throwError "invalid format: setKerneldim dim"
            pure unit
          "setFract" -> do
            case args of
              [fract] -> do
                fract' <- intFromStringE fract
                engineConf <- getEngineConf lib "fract engineConf"
                modLibD lib engineConf _ {fract = fract'}
                compileShaders ssRef esRef lib false

              _ -> throwError "invalid format: setFract fract"
            pure unit
          "setEngineProfile" -> do
            case args of
              [prof] -> do
                elt <- getLibM lib prof
                case (elt :: Maybe EngineConf) of
                  Nothing -> throwError $ "Unknown profile: " <> prof
                  Just d -> do
                    systemConf <- getSystemConf lib "setEngineProfile"
                    modLibD lib systemConf _ {engineConf = prof}

                initEngineST systemST lib uiConfD.canvasId (Just esRef)

              _ -> throwError "invalid format: setProfile profile"
            pure unit
          "clear" -> do
            clearFB engineST
          _ -> throwError $ "Unknown command: " <> msg


-- PRIVATE
save :: forall eff h. (SystemST h) -> PatternD -> EpiS eff h Unit
save systemST patternD = do
  pure unit
  -- pattern
--  id <- lift $ uuid
--  ps <- unsafeSerialize patternSchema (Just id) pattern
--
--  -- modules
--  mods <- (traverse (serializeTup moduleSchema) $ toUnfoldable systemST.moduleRefPool) :: EpiS eff h (Array String)
--  let mres = joinWith "\n\n" mods
--
--  let res = "#PATTERN\n" <> ps <> "\n\n#MODULES\n" <> mres
--  let a = lg res
--
--  lift $ saveCanvas
--
--  pure unit
--
--  where
--    serializeTup :: forall a. Schema -> (Tuple String (STRef h a)) -> EpiS eff h String
--    serializeTup schema (Tuple n ref) = do
--      obj <- lift $ readSTRef ref
--      st <- unsafeSerialize schema (Just n) obj
--      pure st
--
