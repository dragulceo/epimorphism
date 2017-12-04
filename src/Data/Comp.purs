module Data.Comp where

import Prelude
import Data.Kernels (KMap(..), Kernel(..), (<||>))
import Data.Maybe (Maybe(..))
import Graphics.WebGL.Types (WebGLProgram)

foreign import data UniformBindings :: *

type CompST = { src :: KMap (Maybe String), prog :: KMap (Maybe WebGLProgram),
                unif :: KMap (Maybe UniformBindings), aux :: KMap (Maybe (Array String))}

altCST :: CompST -> CompST -> CompST
altCST {src: s0, prog: p0, unif: u0, aux: a0} {src: s1, prog: p1, unif: u1, aux: a1} =
  {src: s0 <||> s1, prog: p0 <||> p1, unif: u0 <||> u1, aux: a0 <||> a1}
infixr 0 altCST as <|||>

newCompST :: CompST
newCompST = { src: KMap Nothing Nothing Nothing Nothing Nothing,
              prog: KMap Nothing Nothing Nothing Nothing Nothing,
              unif: KMap Nothing Nothing Nothing Nothing Nothing,
              aux: KMap Nothing Nothing Nothing Nothing Nothing  }

data CompOp = CompShader Kernel | CompProg Kernel | CompFinish | CompStall
instance showCompOp :: Show CompOp where
  show (CompShader k) = "CompShader: " <> (show k)
  show (CompProg k)   = "CompProg: "   <> (show k)
  show (CompFinish)   = "CompFinish"
  show (CompStall)    = "CompStall"

fullCompile :: Array CompOp
fullCompile = [CompShader Seed1, CompShader Seed0, CompShader Main, CompShader Disp, CompShader Vert,
               CompProg Seed1, CompProg Seed0, CompProg Main, CompProg Disp, CompFinish]
