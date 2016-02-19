module Compiler where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)

import Config
import JSUtil (unsafeURLGet)


type Shaders = { vert :: String, mainFrag :: String, dispFrag :: String }

compileShaders :: forall eff. Pattern -> SystemST -> Epi eff Shaders
compileShaders pattern sys = do
  vert        <- lift $ unsafeURLGet "/shaders/basic.vert.glsl"
  mainFrag    <- lift $ unsafeURLGet "/shaders/main.frag.glsl"
  dispFrag <- lift $ unsafeURLGet "/shaders/display.frag.glsl"

  return {vert, mainFrag, dispFrag}
