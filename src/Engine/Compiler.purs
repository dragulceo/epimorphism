module Compiler where

import Prelude
import Data.TypedArray as T
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (modifySTRef, readSTRef, STRef)
import Control.Monad.Trans.Class (lift)
import Data.Array (foldl, length, nub, uncons)
import Data.Comp (CompOp(..), UniformBindings, newCompST, (<|||>))
import Data.Kernels (Kernel(..), kAcs, kGet, kSet, (<||>))
import Data.Library (delLib, getEngineConfD, getLibM, getPattern, idx, modLibD)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.StrMap (StrMap, empty, insert)
import Data.String (stripPrefix)
import Data.String (Pattern(..)) as S
import Data.Traversable (for)
import Data.Tuple (Tuple(Tuple))
import Data.Types (EngineConfD, EngineProfile, EngineST, EpiS, Library, Pattern(Pattern))
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

  let cST = es.compST
  case (uncons es.compQueue) of
    Just {head: op, tail: rst} -> do
      lift $ log (show op)
      done <- case op of
        CompShader kernel -> do
          Tuple src aux <- parseShader lib (kGet kAcs kernel compD) (globalSubs engineConfD es.profile)
          let cST' = cST { src = (kSet cST.src kernel (Just src)),
                           aux = (kSet cST.aux kernel (Just aux))}
          lift $ modifySTRef esRef _ {compST = cST'}
          pure false
        --CompUploadAux -> do
        CompProg kernel -> do
          case (Tuple (kGet cST.src kernel) (kGet cST.src Vert))  of
            Tuple (Just kernelSrc) (Just vertSrc) -> do
              prog <- execGL es.ctx (compileShadersIntoProgram vertSrc kernelSrc)
              let cST' = cST { prog = (kSet cST.prog kernel (Just prog))}
              lift $ modifySTRef esRef _ {compST = cST'}
              pure false
            _ -> throwError $ "need to compute sources first for " <> (show kernel)
        CompFinish -> do
          -- unif (can probably refactor
          unif <- for cST.prog \prog ->
            case prog of
              Just p -> Just <$> linkShader es p
              Nothing -> pure Nothing

          let cST' = cST { unif = unif <||> cST.unif }

          let newST = cST' <|||> es.curST

          -- aux
          let all_aux = nub $ fromMaybe [] (foldl (<>) Nothing newST.aux)
          uploadAux es all_aux

          -- update engine state with new info
          lift $ modifySTRef esRef _ {curST = newST, currentImages = all_aux}

          -- clean old pattern
          for kAcs \accs ->
            when (accs currentD' /= accs compD) do
              purgeModule lib (accs currentD')

          -- update pattern & reset comp pattern
          modLibD lib currentP (\_ -> compD)
          when ((idx compP).id == "$$Comp") do
            lift $ log "MERGING CLONED PATTERN"
            delLib lib compP

          -- save vertex info
          let new = newCompST
          let new' = new {src  = (kSet new.src  Vert (kGet cST'.src  Vert)),
                          prog = (kSet new.prog Vert (kGet cST'.prog Vert)),
                          unif = (kSet new.unif Vert (kGet cST'.unif Vert))}

          lift $ modifySTRef esRef _ {compST = new'}

          pure true
        CompStall -> do
          pure false

      lift $ modifySTRef esRef _ {compQueue = rst}
      when (length rst /= 0 && full) do
        compileShaders esRef lib full # void

      pure $ done || full
    Nothing -> throwError "shouldn't call compile with an empty queue chump!"


globalSubs :: EngineConfD -> EngineProfile -> StrMap String
globalSubs ec profile =
  let no_fract = profile.angle ||
                 (isJust $ stripPrefix (S.Pattern "Windows") profile.os)
      res   = empty
      res'  = insert "fract" (if no_fract then "1" else (show ec.fract)) res
      res'' = insert "NO_FRACT" (if no_fract then "#define _NO_FRACT_" else "") res'
  in res''


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
