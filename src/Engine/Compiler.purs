module Compiler where

import Prelude
import Data.TypedArray as T
import Config (UniformBindings, EpiS, Pattern, SystemST, EngineST, EngineConf, SystemConf, CompOp(..))
import Control.Monad (when)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (modifySTRef, readSTRef, STRef)
import Control.Monad.Trans (lift)
import Data.Array (length, uncons)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (fst, Tuple(Tuple))
import EngineUtil (execGL)
import Graphics.WebGL.Methods (vertexAttribPointer, enableVertexAttribArray, bindBuffer, bufferData, createBuffer)
import Graphics.WebGL.Shader (getUniformBindings, getAttrBindings, compileShadersIntoProgram, linkProgram)
import Graphics.WebGL.Types (WebGLProgram, DataType(Float), BufferData(DataSource), BufferUsage(StaticDraw), ArrayBufferType(ArrayBuffer))
import Parser (parseShader)
import Texture (uploadAux)
import Util (unsafeCast, lg, now2, now, replaceAll, dbg, Now)

-- compile shaders and load into systemST
compileShaders :: forall eff h. SystemConf -> SystemST h -> EngineConf -> STRef h EngineST -> Pattern -> Boolean -> EpiS (now :: Now | eff) h Unit
compileShaders sysConf systemST engineConf esRef pattern full = do
  dbg "set shaders"

  es <- lift $ readSTRef esRef

  case (uncons es.compQueue) of
    Just {head: op, tail: rst} -> do
      dbg op
      case op of
        CompMainShader -> do
          Tuple main aux <- parseMain systemST pattern engineConf.fract
          lift $ modifySTRef esRef (\es' -> es' {compST = es'.compST {mainSrc = Just main, aux = Just aux}})
          return unit
        CompDispShader -> do
          disp <- parseDisp systemST pattern
          lift $ modifySTRef esRef (\es' -> es' {compST = es'.compST {dispSrc = Just disp}})
          return unit
        CompVertShader -> do
          vert <- parseVert systemST pattern
          lift $ modifySTRef esRef (\es' -> es' {compST = es'.compST {vertSrc = Just vert}})
          return unit
        CompUploadAux -> do
          case es.compST.aux of
            Nothing -> throwError "need to compute aux first!"
            Just aux -> do
              uploadAux es sysConf.host aux
              return unit
        CompMainProg -> do
          case (Tuple es.compST.mainSrc es.compST.vertSrc) of
            Tuple (Just mainSrc) (Just vertSrc) -> do
              mainProg <- execGL es.ctx (compileShadersIntoProgram vertSrc mainSrc)
              lift $ modifySTRef esRef (\es' -> es' {compST = es'.compST {mainProg = Just mainProg}})
              return unit
            _ -> throwError "need to compute sources first!"
        CompDispProg -> do
          case (Tuple es.compST.dispSrc es.compST.vertSrc) of
            Tuple (Just dispSrc) (Just vertSrc) -> do
              dispProg <- execGL es.ctx (compileShadersIntoProgram vertSrc dispSrc)
              lift $ modifySTRef esRef (\es' -> es' {compST = es'.compST {dispProg = Just dispProg}})
              return unit
            _ -> throwError "need to compute sources first!"
        CompBind -> do
          case (Tuple es.compST.mainProg es.compST.dispProg) of
            Tuple (Just mainProg) (Just dispProg) -> do
              Tuple mainUnif dispUnif <- linkShaders es mainProg dispProg
              lift $ modifySTRef esRef (\es' -> es' {compST = es'.compST {mainUnif = Just mainUnif, dispUnif = Just dispUnif}})
              return unit
            _ -> throwError "need to compute programs first!"
        CompFinish -> do
          let es' = es {mainProg = es.compST.mainProg, dispProg = es.compST.dispProg,
                        mainUnif = es.compST.mainUnif, dispUnif = es.compST.dispUnif,
                        currentImages = es.compST.aux}
          lift $ modifySTRef esRef (\_ -> es')
          return unit

      lift $ modifySTRef esRef (\es' -> es' {compQueue = rst})
      when (length rst /= 0 && full) do
        compileShaders sysConf systemST engineConf esRef pattern full

    Nothing -> return unit

  --return unit


parseMain :: forall eff h. SystemST h -> Pattern -> Int -> EpiS eff h (Tuple String (Array String))
parseMain systemST pattern fract = do
  Tuple main' aux <- parseShader systemST pattern.main pattern.includes
  let main = replaceAll "\\$fract\\$" (show fract) main'
  return $ Tuple main aux

parseDisp :: forall eff h. SystemST h -> Pattern -> EpiS eff h String
parseDisp systemST pattern = fst <$> parseShader systemST pattern.disp pattern.includes

parseVert :: forall eff h. SystemST h -> Pattern -> EpiS eff h String
parseVert systemST pattern = fst <$> parseShader systemST pattern.vert []

linkShaders :: forall eff h. EngineST -> WebGLProgram -> WebGLProgram -> EpiS eff h (Tuple UniformBindings UniformBindings)
linkShaders es mainProg dispProg = execGL es.ctx do
  linkProgram mainProg
  linkProgram dispProg
  -- vertex coords
  pos <- createBuffer
  bindBuffer ArrayBuffer pos
  bufferData ArrayBuffer (DataSource (T.asFloat32Array [-1.0,-1.0,1.0,-1.0,-1.0,1.0,
                                                        -1.0,1.0,1.0,-1.0,1.0,1.0])) StaticDraw

  dispAttr <- getAttrBindings dispProg
  mainAttr <- getAttrBindings mainProg

  enableVertexAttribArray mainAttr.a_position
  vertexAttribPointer mainAttr.a_position 2 Float false 0 0
  enableVertexAttribArray dispAttr.a_position
  vertexAttribPointer dispAttr.a_position 2 Float false 0 0

  mainUnif <- getUniformBindings mainProg
  dispUnif <- getUniformBindings dispProg

  return $ Tuple (unsafeCast mainUnif) (unsafeCast dispUnif)
