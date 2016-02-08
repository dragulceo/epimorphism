module Main where

import Prelude
import Data.Maybe
import Data.Either

import Control.Monad.Eff.Console
import Control.Monad.Eff.Alert

import Control.Monad.Eff
--import Control.Monad.Eff.JQuery
import Control.Monad.Trans
import Control.Monad.Except.Trans
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Class

import Graphics.WebGL
import Graphics.WebGL.Types
import Graphics.WebGL.Context
import Graphics.WebGL.Shader
import Graphics.WebGL.Methods
import Graphics.WebGL.Unsafe
import Graphics.Canvas

vertSource :: String
vertSource = """precision mediump float;
asdf
  void main(void) {
    gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
      }
  """

fragSource :: String
fragSource = """
      attribute vec3 aVertexPosition;

      uniform mat4 uMVMatrix;
      uniform mat4 uPMatrix;

      void main(void) {
          gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
      }
  """


initWebGL :: forall r. WebGLProgram -> Object ( |r) -> Object ( |r) -> WebGL Unit
initWebGL prog attr unif = do
  clearColor 0.0 0.0 0.0 1.0

main :: Eff (console :: CONSOLE, alert :: Alert, canvas :: Canvas) Unit
main = do
  Just canvas <- getCanvasElementById "glcanvas"
  Just ctx <- getWebglContext canvas
  res <- (runWebglWithShaders initWebGL) ctx vertSource fragSource
  handleError res

  where
    handleError (Left er) = log $ "WebGL error: " ++ (show er)
    handleError (Right r) = return unit
