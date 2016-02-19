module Compiler where

import Prelude
import Data.Array (sort, length, (..))
import Data.Complex
import Data.Foldable (foldl)
import Data.Int (fromNumber)
import Data.Maybe.Unsafe (fromJust)
import Data.String (replace)
import Data.StrMap (StrMap(), fold, empty, keys, size, foldM, insert)
import Data.Tuple (Tuple(..), snd)

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

type CompRes = { component :: String, zOfs :: Int, parOfs :: Int, parDef :: (Array String) }
compileShaders2 :: forall eff. Pattern -> SystemST -> Epi eff Shaders
compileShaders2 pattern sys = do
  -- get shaders
  -- substitute includes
  mainS <- loadLib pattern.main sys.shaderLib
  let patternM = pattern { component = mainS.body }
  mainRes <- compile patternM sys 0 0 []
  let main = mainRes.component

  dispS <- loadLib pattern.disp sys.shaderLib
  let disp = dispS.body
  vertS <- loadLib pattern.vert sys.shaderLib
  let vert = vertS.body

  return {vert, main, disp}

compile :: forall eff r. {
    modules :: StrMap ModRef
  , par :: StrMap Number
  , zn :: Array Complex
  , sub :: StrMap String
  , component :: String | r } -> SystemST -> Int -> Int -> (Array String) -> Epi eff CompRes
compile {modules, par, zn, sub, component} sys zOfs parOfs parDef = do
  let component' = fold handleSub component sub

  let k = (sort $ keys par)
  let component'' = snd $ foldl handlePar (Tuple parOfs component') k
  let parOfs' = parOfs + (fromJust $ fromNumber $ size par)
  let parDef' = parDef ++ k

  let component''' = snd $ foldl handleZn (Tuple zOfs component'') (0..((length zn) - 1))
  let zOfs' = zOfs + length zn

  mod <- loadModules modules sys.moduleLib
  foldM handleChild { component: component''', zOfs: zOfs', parOfs: parOfs', parDef: parDef'} mod
  where
    handleSub dt k v = replace k v dt
    handlePar (Tuple n dt) v = Tuple (n + 1) (replace ("@" ++ v) ("par[" ++ show n ++ "]") dt)
    handleZn (Tuple n dt) v = Tuple (n + 1) (replace ("#" ++ show v) (show $ v + n) dt)
    handleChild :: forall eff. CompRes -> String -> Module -> Epi eff CompRes
    handleChild {component: componentC, zOfs: zOfsC, parOfs: parOfsC, parDef: parDefC} k v = do
      res <- compile v sys zOfsC parOfsC parDefC
      let child = replace ("@@" ++ k) res.component componentC
      return $ res { component = child }
