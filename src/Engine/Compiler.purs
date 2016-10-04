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
import Graphics.WebGL.Shader (getAttrBindings, compileShadersIntoProgram)
import Graphics.WebGL.Types (DataType(Float), BufferData(DataSource), BufferUsage(StaticDraw), ArrayBufferType(ArrayBuffer))
import Parser (parseShaders)
import Texture (uploadAux)
import Util (now2, now, replaceAll, dbg, Now)

-- compile shaders and load into systemST
compileShaders :: forall eff h. SystemConf -> SystemST h -> EngineConf -> STRef h EngineST -> Pattern -> EpiS (now :: Now | eff) h Unit
compileShaders sysConf systemST engineConf esRef pattern = do
  dbg "set shaders"
  t0 <- lift now
  es <- lift $ readSTRef esRef

  -- load & parse shaders
  {main, disp, vert, aux} <- parseShaders pattern systemST
  let mainF = replaceAll "\\$fract\\$" (show engineConf.fract) main -- a little ghetto, we need this in a for loop

  t1 <- lift now
  auxImg <- uploadAux es sysConf.host aux
  t2 <- lift now

  Tuple main' disp' <- execGL es.ctx ( do
    -- create programs
    a0 <- lift $ lift now2
    mainProg <- compileShadersIntoProgram vert mainF
    a1 <- lift $ lift now2
    dispProg <- compileShadersIntoProgram vert disp
    a2 <- lift $ lift now2
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
    a3 <- lift $ lift now2
    --let b = lg $ inj " c0:%0ms\n c1:%1ms\n rst:%2ms" [show (a1 - a0), show (a2 - a1), show (a3 - a2)]
    return $ Tuple mainProg dispProg
  )

  lift $ modifySTRef esRef (\s -> s {dispProg = Just disp', mainProg = Just main', auxImg = auxImg})
  t3 <- lift now

  --dbg $ inj "splice:%0ms\nimg:%1ms\ncompile:%2ms\ntotal:%3ms" [show (t1 - t0), show (t2 - t1), show (t3 - t2), show (t3 - t0)]
  return unit
