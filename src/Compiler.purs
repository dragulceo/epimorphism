module Compiler where

import Prelude
import Data.Array (sort, length, (..))
import Data.Complex
import Data.Foldable (foldl)
import Data.Int (fromNumber)
import Data.Maybe.Unsafe (fromJust)
import Data.String (joinWith)
import Data.StrMap (StrMap(), fold, empty, keys, size, foldM, insert)
import Data.Tuple (Tuple(..), snd)
import Data.Traversable (traverse)

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)

import Config
import System
import JSUtil (replaceAll, unsafeURLGet, reallyUnsafeLog)

type Shaders = { vert :: String, main :: String, disp :: String }
type CompRes = { component :: String, zOfs :: Int, parOfs :: Int }

compileShaders :: forall eff. Pattern -> SystemST -> Epi eff Shaders
compileShaders pattern sys = do
  vertM   <- loadLib pattern.vert sys.moduleLib
  vertRes <- compile vertM sys 0 0

  dispM   <- loadLib pattern.disp sys.moduleLib
  dispRes <- compile dispM sys 0 0

  mainM   <- loadLib pattern.main sys.moduleLib
  mainRes <- compile mainM sys 0 0

  -- substitute includes into frags
  includes <- traverse (\x -> loadLib x sys.componentLib) pattern.includes
  let allIncludes = (joinWith "\n\n" (map (\x -> x.body) includes)) ++ "\n\n"

  return {vert: vertRes.component, main: (allIncludes ++ mainRes.component), disp: (allIncludes ++ dispRes.component)}


compile :: forall eff. Module -> SystemST -> Int -> Int -> Epi eff CompRes
compile mod sys zOfs parOfs = do
  comp <- loadLib mod.component sys.componentLib
  let component' = fold handleSub comp.body mod.sub
  let k = (sort $ keys mod.par)

  let component'' = snd $ foldl handlePar (Tuple parOfs component') k
  let parOfs' = parOfs + (fromJust $ fromNumber $ size mod.par)

  let component''' = foldl handleZn component'' (0..((length mod.zn) - 1))
  let zOfs' = zOfs + length mod.zn

  mod <- loadModules mod.modules sys.moduleLib
  foldM handleChild { component: component''', zOfs: zOfs', parOfs: parOfs' } mod
  where
    handleSub dt k v = replaceAll ("\\$" ++ k) v dt
    handlePar (Tuple n dt) v = Tuple (n + 1) (replaceAll ("@" ++ v) ("par[" ++ show n ++ "]") dt)
    handleZn dt v = replaceAll ("#" ++ show v) (show $ (v + zOfs)) dt
    handleChild :: forall eff. CompRes -> String -> Module -> Epi eff CompRes
    handleChild { component: componentC, zOfs: zOfsC, parOfs: parOfsC } k v = do
      res <- compile v sys zOfsC parOfsC
      let child = replaceAll ("@@" ++ k) ("{\n" ++ res.component ++ "\n}\n") componentC
      return $ res { component = child }
