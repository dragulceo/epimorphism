module Compiler where

import Prelude
import Data.StrMap
import Data.Complex
import Data.Tuple (Tuple(..))

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)

import Config
import System
import JSUtil (unsafeURLGet)


type Shaders = { vert :: String, main :: String, disp :: String }

compileShaders :: forall eff. Pattern -> SystemST -> Epi eff Shaders
compileShaders pattern sys = do
  vert <- lift $ unsafeURLGet "/shaders/basic.vert.glsl"
  main <- lift $ unsafeURLGet "/shaders/main.frag.glsl"
  disp <- lift $ unsafeURLGet "/shaders/display.frag.glsl"

  return {vert, main, disp}

type CompRes = { component :: String, zOfs :: Int, parOfs :: Int }
compileShaders2 :: forall eff. Pattern -> SystemST -> Epi eff Shaders
compileShaders2 pattern sys = do
  -- get shaders
  mainS <- loadLib pattern.main sys.shaderLib
  let patternM = pattern { component = mainS.body }
  mainRes <- compile patternM sys 0 0
  let main = mainRes.component

  dispS <- loadLib pattern.disp sys.shaderLib
  let disp = dispS.body
  vertS <- loadLib pattern.vert sys.shaderLib
  let vert = vertS.body

  return {vert, main, disp}

compile :: forall eff r. {
    modules :: SubModules
  , par :: StrMap Number
  , zn :: Array Complex
  , sub :: StrMap String
  , component :: String | r } -> SystemST -> Int -> Int-> Epi eff CompRes
compile {modules, par, zn, component} sys zOfs parOfs = do
  -- substitute includes

  -- substitute substitutions
  -- substitute par
  -- substitute zn
  -- substitute children
  return {component, zOfs: 0, parOfs: 0}
