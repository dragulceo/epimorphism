module Main where

import Prelude
import Data.Either
import Data.Maybe
import Data.Tuple
import Data.Array
import Data.TypedArray as T
import Data.Int (toNumber)
import Data.Function

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Alert
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Class
import Control.Monad.Reader.Class (ask)
import Control.Monad.Eff.Class (liftEff)

import Graphics.WebGL
import Graphics.WebGL.Types
import Graphics.WebGL.Context
import Graphics.WebGL.Shader
import Graphics.WebGL.Methods
import Graphics.Canvas
import Graphics.WebGL.Raw as GL
import Graphics.WebGL.Raw.Enums as GLE
import Graphics.WebGL.Raw.Types as GLT

foreign import getURL :: String -> String
foreign import unsafeNull :: forall a. a
foreign import requestAnimationFrame :: forall eff a. Eff (console :: CONSOLE | eff) (Either WebGLError a) -> Eff ( | eff) Unit


initTex :: Int -> WebGL (Tuple WebGLTexture WebGLFramebuffer)
initTex dim = do
  ctx <- ask
  tex <- createTexture
  fb  <- createFramebuffer
  liftEff $ GL.bindTexture ctx GLE.texture2d tex
  liftEff $ GL.texParameteri ctx GLE.texture2d GLE.textureWrapS GLE.clampToEdge
  liftEff $ GL.texParameteri ctx GLE.texture2d GLE.textureWrapT GLE.clampToEdge
  liftEff $ GL.texParameteri ctx GLE.texture2d GLE.textureMinFilter GLE.linear
  liftEff $ GL.texParameteri ctx GLE.texture2d GLE.textureMagFilter GLE.linear
  liftEff $ GL.texImage2D_ ctx GLE.texture2d 0 GLE.rgba dim dim 0 GLE.rgba GLE.unsignedByte (unsafeNull :: GLT.ArrayBufferView)
  liftEff $ GL.bindFramebuffer ctx GLE.framebuffer fb
  liftEff $ GL.framebufferTexture2D ctx GLE.framebuffer GLE.colorAttachment0 GLE.texture2d tex 0

  return $ Tuple tex fb

type ProgData = { displayProg :: WebGLProgram, mainProg :: WebGLProgram, ts :: (Tuple WebGLTexture WebGLTexture) , fbs :: (Tuple WebGLFramebuffer WebGLFramebuffer) }

initWebGL :: Int -> WebGL ProgData
initWebGL dim = do
  ctx <- ask

  -- initialize shaders
  let basicVert   = getURL "/shaders/basic.vert.glsl"
  let mainFrag    = getURL "/shaders/main.frag.glsl"
  let displayFrag = getURL "/shaders/display.frag.glsl"

  mainProg    <- compileShadersIntoProgram basicVert mainFrag
  displayProg <- compileShadersIntoProgram basicVert displayFrag
  displayAttr <- getAttrBindings displayProg
  mainAttr    <- getAttrBindings displayProg

  -- -- initialize textures
  Tuple t0 fb0 <- initTex dim
  Tuple t1 fb1 <- initTex dim

  -- vertex coords
  pos <- createBuffer
  bindBuffer ArrayBuffer pos
  bufferData ArrayBuffer (DataSource (T.asFloat32Array [-1.0,-1.0,1.0,-1.0,-1.0, 1.0, -1.0,1.0,1.0,-1.0,1.0,1.0])) StaticDraw

  enableVertexAttribArray mainAttr.a_position
  vertexAttribPointer mainAttr.a_position 2 Float false 0 0
  enableVertexAttribArray displayAttr.a_position
  vertexAttribPointer displayAttr.a_position 2 Float false 0 0

  -- misc
  canvasWidth <- drawingBufferWidth
  canvasHeight <- drawingBufferWidth
  liftEff $ GL.viewport ctx 0 0 canvasWidth canvasHeight
  clearColor 0.0 0.0 0.0 1.0
  liftEff $ GL.clear ctx GLE.colorBufferBit
  return { displayProg: displayProg, mainProg: mainProg, ts: (Tuple t0 t1), fbs: (Tuple fb0 fb1) }

animate :: ProgData -> Int -> WebGL Unit
animate pd count = do
  ctx <- ask

  -- ping pong buffers
  let tm = if count `mod` 2 == 0 then fst pd.ts else snd pd.ts
  let td = if count `mod` 2 == 1 then fst pd.ts else snd pd.ts
  let fb = if count `mod` 2 == 1 then fst pd.fbs else snd pd.fbs

  -- main program
  liftEff $ GL.useProgram ctx pd.mainProg
  liftEff $ GL.bindTexture ctx GLE.texture2d tm
  liftEff $ GL.bindFramebuffer ctx GLE.framebuffer fb

  mainUnif <- getUniformBindings pd.mainProg
  uniform1f mainUnif.kernel_dim 1024.0
  uniform1f mainUnif.time (toNumber count / 45.0)

  drawArrays Triangles 0 6

  -- display/post program
  liftEff $ GL.useProgram ctx pd.displayProg
  liftEff $ GL.bindTexture ctx GLE.texture2d td
  liftEff $ GL.bindFramebuffer ctx GLE.framebuffer unsafeNull

  displayUnif <- getUniformBindings pd.displayProg
  uniform1f displayUnif.kernel_dim 1024.0
  drawArrays Triangles 0 6

  liftEff $ requestAnimationFrame $ runWebgl (animate pd (count + 1)) ctx


main :: Eff (console :: CONSOLE, alert :: Alert, canvas :: Canvas) Unit
main = do
  Just canvas <- getCanvasElementById "glcanvas"
  Just ctx <- getWebglContext canvas
  res <- runWebgl ( do
    progData <- initWebGL 1024
    return unit
    animate progData 0
  ) ctx

  handleError res

  where
    handleError (Left er) = log $ show er
    handleError (Right r) = return unit
