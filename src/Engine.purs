module Engine where

import Prelude
import Data.Tuple
import Data.Maybe

import Data.Either
import Data.Array
import Data.TypedArray as T
import Data.Int (toNumber)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT (), lift)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.ST

import Graphics.WebGL
import Graphics.WebGL.Types
import Graphics.WebGL.Raw as GL
import Graphics.WebGL.Raw.Enums as GLE
import Graphics.WebGL.Raw.Types as GLT
import Graphics.WebGL.Context
import Graphics.WebGL.Shader
import Graphics.WebGL.Methods
import Graphics.Canvas (Canvas, getCanvasElementById, setCanvasWidth, setCanvasHeight)

import Config
import Compiler
import JSUtil (unsafeLog, unsafeNull, unsafeURLGet)

-- PUBLIC
-- this might throw an error
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

-- this might throw an error
setShaders :: forall eff h. (STRef h EngineST) -> SystemST -> Pattern -> Epi (st :: ST h | eff) Unit
setShaders esRef sys pattern = do
  es <- lift $ readSTRef esRef

  -- load & compile shaders
  {mainFrag: mainFrag, dispFrag: dispFrag, vert: vert} <- compileShaders pattern sys

  Tuple main disp <- execGL es.ctx ( do
    -- creater programs
    mainProg    <- compileShadersIntoProgram vert mainFrag
    dispProg <- compileShadersIntoProgram vert dispFrag
    dispAttr <- getAttrBindings dispProg
    mainAttr    <- getAttrBindings mainProg

    -- vertex coords
    pos <- createBuffer
    bindBuffer ArrayBuffer pos
    bufferData ArrayBuffer (DataSource (T.asFloat32Array [-1.0,-1.0,1.0,-1.0,-1.0, 1.0, -1.0,1.0,1.0,-1.0,1.0,1.0])) StaticDraw

    enableVertexAttribArray mainAttr.a_position
    vertexAttribPointer mainAttr.a_position 2 Float false 0 0
    enableVertexAttribArray dispAttr.a_position
    vertexAttribPointer dispAttr.a_position 2 Float false 0 0

    return $ Tuple mainProg dispProg
  )

  lift $ modifySTRef esRef (\s -> s { dispProg = Just disp, mainProg = Just main })
  return unit


-- this might throw an error
initEngineST :: forall h eff. EngineConf -> SystemST -> Pattern -> String -> Epi (st :: ST h | eff) (STRef h EngineST)
initEngineST engineConf sys pattern canvasId = do
  -- these are unsafe
  Just canvas <- liftEff $ getCanvasElementById canvasId
  Just ctx <- liftEff $ getWebglContext canvas

  let es = {dispProg: Nothing, mainProg: Nothing, tex: Nothing, fb: Nothing, ctx: ctx}
  esRef <- lift $ newSTRef es

  let dim = engineConf.kernelDim

  -- if we change kernel_dim we need to redo this
  lift $ setCanvasWidth (toNumber dim) canvas
  lift $ setCanvasHeight (toNumber dim) canvas

  setShaders esRef sys pattern
  es' <- lift $ readSTRef esRef

  res <- execGL ctx ( do
    Tuple tex0 fb0 <- initTex dim
    Tuple tex1 fb1 <- initTex dim

    clearColor 0.0 0.0 0.0 1.0
    liftEff $ GL.clear ctx GLE.colorBufferBit
    liftEff $ GL.viewport ctx 0 0 dim dim
    return $ es' {
        fb = (Just (Tuple fb0 fb1))
      , tex = (Just (Tuple tex0 tex1))
    }
  )
  lift $ newSTRef res


render :: forall h eff. EngineConf -> EngineST -> Pattern -> Int -> Epi (st :: ST h | eff) Unit
render engineConf engineST pattern frameNum = do
  let ctx = engineST.ctx

  -- unpack
  tex <- case engineST.tex of
    (Just x) -> return x
    otherwise -> throwError "Render: missing textures"
  fbs <- case engineST.fb of
    (Just x) -> return x
    otherwise -> throwError "Render: missing framebuffers"
  main <- case engineST.mainProg of
    (Just x) -> return x
    otherwise -> throwError "Render: missing main program"
  disp <- case engineST.dispProg of
    (Just x) -> return x
    otherwise -> throwError "Render: missing disp program"

  execGL ctx ( do
    -- ping pong buffers
    let tm = if frameNum `mod` 2 == 0 then fst tex else snd tex
    let td = if frameNum `mod` 2 == 1 then fst tex else snd tex
    let fb = if frameNum `mod` 2 == 1 then fst fbs else snd fbs

    -- main program
    liftEff $ GL.useProgram ctx main
    liftEff $ GL.bindTexture ctx GLE.texture2d tm
    liftEff $ GL.bindFramebuffer ctx GLE.framebuffer fb

    -- bind main uniforms
    mainUnif <- getUniformBindings main
    uniform1f mainUnif.time ((pattern.t - pattern.tPhase) / 1000.0)
    uniform1f mainUnif.kernel_dim (toNumber engineConf.kernelDim)
    drawArrays Triangles 0 6

    -- disp/post program
    liftEff $ GL.useProgram ctx disp
    liftEff $ GL.bindTexture ctx GLE.texture2d td
    liftEff $ GL.bindFramebuffer ctx GLE.framebuffer unsafeNull

    -- bind disp uniforms
    dispUnif <- getUniformBindings disp
    uniform1f dispUnif.kernel_dim (toNumber engineConf.kernelDim)
    drawArrays Triangles 0 6
  )


-- PRIVATE
execGL :: forall eff a. WebGLContext -> (WebGL a) -> Epi eff a
execGL ctx webGL = do
  res <- lift $ runWebgl webGL ctx
  case res of
    Left er -> throwError $ show er
    Right ret -> return ret
