module Command where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (STRef, ST, modifySTRef, readSTRef)
import DOM (DOM)
import Data.Array (uncons, updateAt, length)
import Data.Library (buildSearch, getEngineConf, getLib, getLibM, getPatternD, getSystemConf, getUIConf, getUIConfD, idM, modLibD, modLibD', searchLib, dat)
import Data.Maybe (Maybe(..))
import Data.StrMap (fromFoldable, insert, toUnfoldable)
import Data.String (joinWith, split)
import Data.String (Pattern(..)) as S
import Data.System (EngineST, SystemST, UIST)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst)
import Data.Types (EngineConf, EpiS, Library, Module(..), PatternD)
import Engine.Compiler (compileShaders)
import Engine.Engine (initEngineST)
import Engine.Texture (clearFB)
import Graphics.Canvas (CANVAS)
import Paths (runPath)
import Pattern (findModule)
import Script.ScriptUtil (addScript)
import UI.Layout (updateLayout, initLayout)
import Util (Now, enableDebug, halt, handleError, intFromStringE, log, real, replaceAll)

foreign import saveCanvas :: forall eff. Eff eff Unit

command :: forall eff h. STRef h UIST -> STRef h EngineST -> STRef h (SystemST h) -> Library h -> String -> Eff (canvas :: CANVAS, dom :: DOM, st :: ST h, now :: Now | eff) Unit
command usRef esRef ssRef lib msg = handleError do
  uiConfD    <- getUIConfD lib "command uiConf"
  patternD   <- getPatternD lib "command pattern"

  systemST   <- lift $ readSTRef ssRef
  uiST       <- lift $ readSTRef usRef
  engineST   <- lift $ readSTRef esRef

  --winLog $ "EXECUTE: " <> (show msg)
  lift $ log $ "EXECUTE: " <> (show msg)

  let dt = split (S.Pattern " ") msg

  unless (length dt == 0) do
    case uncons dt of
      Nothing -> throwError $ "invalid message " <> msg
      Just {head: cmd, tail: args} -> do
        case cmd of
          "null" -> pure unit
          "pause" -> do
            lift $ log "pause"
            lift $ modifySTRef ssRef _ {paused = not systemST.paused}
            pure unit
          "next" -> do
            lift $ modifySTRef ssRef _ {paused = false, next = true}
            pure unit
          "halt" -> do
            lift $ halt
            pure unit
          "pauseAfterSwitch" -> do
            lift $ modifySTRef ssRef _ {pauseAfterSwitch = true}
            pure unit
          "killScripts" -> do
            let sch = buildSearch ["live"] [] []
            modules <- searchLib lib sch

            for (modules :: Array Module) \m -> do
              modLibD lib m _ {scripts = []}

            pure unit
          "killPaths" -> do
            let sch = buildSearch ["live"] [] []
            modules <- searchLib lib sch

            for (modules :: Array Module) \m -> do
              zn <- for (dat m).zn \val -> do
                show <$> fst <$> runPath systemST.t val

              let unf = toUnfoldable (dat m).par :: Array (Tuple String String)
              par' <- for unf \(Tuple n val) -> do
                Tuple n <$> show <$> real <$> fst <$> runPath systemST.t val

              let par = fromFoldable par'
              modLibD lib m _ { zn = zn, par = par}

            pure unit
          "scr" -> do
            case uncons args of
              Nothing -> throwError $ "invalid script args " <> msg
              Just {head: addr, tail: rst} -> do
                case uncons rst of
                  Nothing -> throwError $ "invalid script args " <> msg
                  Just {head: name, tail: rst'} -> do
                    mid <- findModule lib patternD addr true
                    addScript lib systemST.t mid name (joinWith " " rst')
                    pure unit
          "setP" -> do
            case args of
              [addr, par, val] -> do
                let val' = replaceAll "___" " " val
                mid <- findModule lib patternD addr true

                modLibD' lib idM mid "find module - setP" $ \m ->
                  m {par = insert par val' m.par}

              _ -> throwError "invalid format: setP addr par val"
          "setZn" -> do
            case args of
              [addr, idx, val] -> do
                let val' = replaceAll "___" " " val
                idx' <- intFromStringE idx
                mid  <- findModule lib patternD addr true

                mod@(Module _ modD) <- getLib lib mid "find module - setZn"
                zn' <- case (updateAt idx' val' modD.zn) of
                  Just x -> pure x
                  _ -> throwError "zn idx out of bounds setZn"
                modLibD lib mod _ {zn = zn'}

                pure unit
              _ -> throwError "invalid format: setPar addr par val"
          "setT" -> do
            let tExp = "" -- joinWith "" args
            mid <- findModule lib patternD "main.application.t.t_inner" true

            modLibD' lib idM mid "find module - setT" $ \m ->
              m {sub = insert "t_expr" tExp m.sub}

            compileShaders esRef lib false

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
            lift $ enableDebug
            initLayout uiST lib
            pure unit
          "initLayout" -> do
            initLayout uiST lib
            pure unit
          "updateLayout" -> do
            updateLayout uiST systemST engineST lib true
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
                initEngineST lib uiConfD.canvasId (Just esRef)

              _ -> throwError "invalid format: setKerneldim dim"
            pure unit
          "setFract" -> do
            case args of
              [fract] -> do
                fract' <- intFromStringE fract
                engineConf <- getEngineConf lib "fract engineConf"
                modLibD lib engineConf _ {fract = fract'}
                compileShaders esRef lib false

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

                initEngineST lib uiConfD.canvasId (Just esRef)

              _ -> throwError "invalid format: setProfile profile"
            pure unit
          "clear" -> do
            clearFB engineST
          _ -> throwError $ "Unknown command: " <> msg


-- PRIVATE
save :: forall eff h. (SystemST h) -> PatternD -> EpiS eff h Unit
save systemST patternD = do
  lift $ saveCanvas
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
