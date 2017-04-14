module Compiler where

import Prelude
import Data.TypedArray as T
import Control.Alt ((<|>))
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (modifySTRef, readSTRef, STRef)
import Control.Monad.Trans.Class (lift)
import Data.Array (length, uncons)
import Data.Library (delLib, getEngineConfD, getLib, getLibM, getPattern, idx, modLibD)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (stripPrefix)
import Data.String (Pattern(..)) as S
import Data.Tuple (fst, Tuple(Tuple))
import Data.Types (CompOp(..), Component(..), EngineST, EpiS, Library, Pattern(..), PatternD, SystemST, UniformBindings, newCompST)
import EngineUtil (execGL)
import Graphics.WebGL.Methods (vertexAttribPointer, enableVertexAttribArray, bindBuffer, bufferData, createBuffer)
import Graphics.WebGL.Shader (getUniformBindings, getAttrBindings, compileShadersIntoProgram, linkProgram)
import Graphics.WebGL.Types (WebGLProgram, DataType(Float), BufferData(DataSource), BufferUsage(StaticDraw), ArrayBufferType(ArrayBuffer))
import Parser (parseShader)
import Pattern (purgeModule)
import Texture (uploadAux)
import Util (Now, log, now, now2, replaceAll, unsafeCast, winLog)

-- compile shaders and load into systemST
compileShaders :: forall eff h. STRef h EngineST -> Library h -> Boolean -> EpiS (now :: Now | eff) h Boolean
compileShaders esRef lib full = do
  es <- lift $ readSTRef esRef
  engineConfD <- getEngineConfD lib "compileShaders"

  currentP@(Pattern _ currentD') <- getPattern lib "patternD compileShaders"
  compP@(Pattern _ compD) <- fromMaybe currentP <$> getLibM lib "$$Comp"

  case (uncons es.compQueue) of
    Just {head: op, tail: rst} -> do
      lift $ log op
      done <- case op of
        CompMainShader -> do
          let no_fract = es.profile.angle ||
                         (isJust $ stripPrefix (S.Pattern "Windows") es.profile.os)
          let fract = if no_fract then Nothing else Just engineConfD.fract
          Tuple main aux <- parseMain lib compD fract
          lift $ modifySTRef esRef (\es' -> es' {compST = es'.compST {mainSrc = Just main, auxImages = Just aux}})
          pure false
        CompDispShader -> do
          disp <- parseDisp lib compD
          lift $ modifySTRef esRef (\es' -> es' {compST = es'.compST {dispSrc = Just disp}})
          pure false
        CompVertShader -> do
          vert <- parseVert lib compD
          lift $ modifySTRef esRef (\es' -> es' {compST = es'.compST {vertSrc = Just vert}})
          pure false
        --CompUploadAux -> do
        CompMainProg -> do
          case (Tuple es.compST.mainSrc es.compST.vertSrc) of
            Tuple (Just mainSrc) (Just vertSrc) -> do
              mainProg <- execGL es.ctx (compileShadersIntoProgram vertSrc mainSrc)
              lift $ modifySTRef esRef (\es' -> es' {compST = es'.compST {mainProg = Just mainProg}})
              pure false
            _ -> throwError "need to compute sources first!"
        CompDispProg -> do
          case (Tuple es.compST.dispSrc es.compST.vertSrc) of
            Tuple (Just dispSrc) (Just vertSrc) -> do
              dispProg <- execGL es.ctx (compileShadersIntoProgram vertSrc dispSrc)
              lift $ modifySTRef esRef (\es' -> es' {compST = es'.compST {dispProg = Just dispProg}})
              pure false
            _ -> throwError "need to compute sources first!"
        CompFinish -> do
          -- aux
          case es.compST.auxImages of
            Just aux -> do
              uploadAux es aux
            Nothing -> pure unit

          -- unif
          mainUnif <- case es.compST.mainProg of
            Just prog -> do
              Just <$> linkShader es prog
            Nothing -> pure Nothing

          dispUnif <- case es.compST.dispProg of
            Just prog -> do
              Just <$> linkShader es prog
            Nothing -> pure Nothing

          -- update engine state with new info
          lift $ modifySTRef esRef (\es' ->
                                     es' {mainProg = es'.compST.mainProg <|> es'.mainProg,
                                          dispProg = es'.compST.dispProg <|> es'.dispProg,
                                          mainUnif = mainUnif <|> es'.mainUnif,
                                          dispUnif = dispUnif <|> es'.dispUnif,
                                          currentImages = es.compST.auxImages <|> es'.currentImages})

          -- clean old pattern
          when (currentD'.main /= compD.main) do
            purgeModule lib currentD'.main
          when (currentD'.disp /= compD.disp) do
            purgeModule lib currentD'.disp
          when (currentD'.vert /= compD.vert) do
            purgeModule lib currentD'.vert

          -- update pattern & reset comp info
          modLibD lib currentP (\_ -> compD)
          when ((idx compP).id == "$$Comp") do
            delLib lib compP

          lift $ modifySTRef esRef (\es' -> es' {compST = es'.compST {mainProg=Nothing, dispProg=Nothing}})

          pure true
        CompStall -> do
          pure false

      lift $ modifySTRef esRef (\es' -> es' {compQueue = rst})
      when (length rst /= 0 && full) do
        compileShaders esRef lib full
        pure unit

      pure $ done || full
    Nothing -> throwError "shouldn't call compile with an empty queue chump!"


parseMain :: forall eff h. Library h -> PatternD -> Maybe Int -> EpiS eff h (Tuple String (Array String))
parseMain lib patternD fract = do
  Tuple main'' aux <- parseShader lib patternD.main patternD.includes

  -- kind of ghetto
  main <- case fract of
    Just i -> do
      let main' = replaceAll "~fract~" (show i) main''
      pure $ replaceAll "~NO_FRACT~" "" main'
    Nothing -> do
      lift $ log "NO FRACT"
      let main' = replaceAll "~fract~" "1" main''
      pure $ replaceAll "~NO_FRACT~" "#define _NO_FRACT_" main'

  pure $ Tuple main aux

parseDisp :: forall eff h. Library h -> PatternD -> EpiS eff h String
parseDisp lib patternD = fst <$> parseShader lib patternD.disp patternD.includes

parseVert :: forall eff h. Library h -> PatternD -> EpiS eff h String
parseVert lib patternD = fst <$> parseShader lib patternD.vert []

linkShader :: forall eff h. EngineST -> WebGLProgram -> EpiS eff h UniformBindings
linkShader es prog = execGL es.ctx do
  pos <- createBuffer
  bindBuffer ArrayBuffer pos
  bufferData ArrayBuffer (DataSource (T.asFloat32Array [-1.0,-1.0,1.0,-1.0,-1.0,1.0,
                                                        -1.0,1.0,1.0,-1.0,1.0,1.0])) StaticDraw

  linkProgram prog
  attr <- getAttrBindings prog
  enableVertexAttribArray attr.a_position
  vertexAttribPointer attr.a_position 2 Float false 0 0
  unif <- getUniformBindings prog
  pure (unsafeCast unif)
