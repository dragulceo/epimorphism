module Compiler where

import Prelude
import Data.TypedArray as T
import Config (UniformBindings, EpiS, Pattern, SystemST, EngineST, EngineConf, SystemConf)
import Control.Monad.ST (modifySTRef, readSTRef, STRef)
import Control.Monad.Trans (lift)
import Data.Maybe (Maybe(Just))
import Data.Tuple (fst, Tuple(Tuple))
import EngineUtil (execGL)
import Graphics.WebGL.Methods (vertexAttribPointer, enableVertexAttribArray, bindBuffer, bufferData, createBuffer)
import Graphics.WebGL.Shader (getUniformBindings, getAttrBindings, compileShadersIntoProgram, linkProgram)
import Graphics.WebGL.Types (WebGLProgram, DataType(Float), BufferData(DataSource), BufferUsage(StaticDraw), ArrayBufferType(ArrayBuffer))
import Parser (parseShader)
import Texture (uploadAux)
import Util (unsafeCast, lg, now2, now, replaceAll, dbg, Now)

-- compile shaders and load into systemST
compileShaders :: forall eff h. SystemConf -> SystemST h -> EngineConf -> STRef h EngineST -> Pattern -> EpiS (now :: Now | eff) h Unit
compileShaders sysConf systemST engineConf esRef pattern = do
  dbg "set shaders"
  es <- lift $ readSTRef esRef

  Tuple main aux <- parseMain systemST pattern engineConf.fract
  disp <- parseDisp systemST pattern
  vert <- parseVert systemST pattern

  auxImg <- uploadAux es sysConf.host aux

  mainProg <- execGL es.ctx (compileShadersIntoProgram vert main)

  dispProg <- execGL es.ctx (compileShadersIntoProgram vert disp)

  Tuple mainUnif' dispUnif' <- linkShaders es mainProg dispProg

  lift $ modifySTRef esRef (\s -> s {dispProg = Just dispProg, mainProg = Just mainProg,
    mainUnif = Just mainUnif', dispUnif = Just dispUnif',
    auxImg = auxImg})

  return unit


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
