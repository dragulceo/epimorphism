module Compiler where

import Prelude
import Data.TypedArray as T
import Config (EpiS, Pattern, SystemST, EngineST, EngineConf, SystemConf)
import Control.Monad.ST (modifySTRef, readSTRef, STRef)
import Control.Monad.Trans (lift)
import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple(Tuple))
import EngineUtil (execGL)
import Graphics.WebGL.Methods (vertexAttribPointer, enableVertexAttribArray, bindBuffer, bufferData, createBuffer)
import Graphics.WebGL.Shader (getAttrBindings, compileShadersIntoProgram, linkProgram)
import Graphics.WebGL.Types (DataType(Float), BufferData(DataSource), BufferUsage(StaticDraw), ArrayBufferType(ArrayBuffer))
import Parser (parseShader)
import Texture (uploadAux)
import Util (now2, now, replaceAll, dbg, Now)

-- compile shaders and load into systemST
compileShaders :: forall eff h. SystemConf -> SystemST h -> EngineConf -> STRef h EngineST -> Pattern -> EpiS (now :: Now | eff) h Unit
compileShaders sysConf systemST engineConf esRef pattern = do
  dbg "set shaders"
  es <- lift $ readSTRef esRef

  -- load & parse shaders

  Tuple main' aux <- parseShader systemST pattern.main pattern.includes
  let main = replaceAll "\\$fract\\$" (show engineConf.fract) main'

  Tuple disp _   <- parseShader systemST pattern.disp pattern.includes

  Tuple vert _   <- parseShader systemST pattern.vert []

  auxImg <- uploadAux es sysConf.host aux

  mainProg <- execGL es.ctx (compileShadersIntoProgram vert main)
  dispProg <- execGL es.ctx (compileShadersIntoProgram vert disp)

  execGL es.ctx ( do
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
  )

  lift $ modifySTRef esRef (\s -> s {dispProg = Just dispProg, mainProg = Just mainProg, auxImg = auxImg})

  return unit
