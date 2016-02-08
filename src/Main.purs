module Main where

import Prelude
import Data.Maybe
import Data.Either

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Alert
import Control.Monad.Trans
import Control.Monad.Except.Trans
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Class
import Control.Monad.Reader.Class (ask)
import Control.Monad.Eff.Class (liftEff)

import Graphics.WebGL
import Graphics.WebGL.Types
import Graphics.WebGL.Context
import Graphics.WebGL.Shader
import Graphics.WebGL.Methods
import Graphics.WebGL.Unsafe
import Graphics.Canvas
import Graphics.WebGL.Raw as GL
import Graphics.WebGL.Raw.Enums as GLE

foreign import getURL :: String -> String

initWebGL :: WebGL Unit
initWebGL = do
  ctx <- ask

  clearColor 0.0 0.0 0.0 1.0
  canvasWidth <- drawingBufferWidth
  canvasHeight <- drawingBufferWidth

  liftEff $ GL.viewport ctx 0 0 canvasWidth canvasHeight
  liftEff $ GL.clear ctx GLE.colorBufferBit

  let basicVert   = getURL "/shaders/basic.vert.glsl"
  let mainFrag    = getURL "/shaders/main.frag.glsl"
  let displayFrag = getURL "/shaders/display.frag.glsl"

  displayProg <- compileShadersIntoProgram basicVert displayFrag
  mainProg    <- compileShadersIntoProgram basicVert mainFrag

  liftEff $ GL.viewport ctx 0 0 canvasWidth canvasHeight
  liftEff $ GL.clear ctx GLE.colorBufferBit



main :: Eff (console :: CONSOLE, alert :: Alert, canvas :: Canvas) Unit
main = do
  Just canvas <- getCanvasElementById "glcanvas"
  Just ctx <- getWebglContext canvas
  res <- runWebgl initWebGL ctx
  handleError res

  where
    handleError (Left er) = log $ show er
    handleError (Right r) = return unit
