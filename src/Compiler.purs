module Compiler where

import Prelude
import Data.Array (sort, length, foldM, (..)) as A
import Data.Complex
import Data.Foldable (foldl)
import Data.Int (fromNumber)
import Data.List (fromList)
import Data.Maybe.Unsafe (fromJust)
import Data.String (joinWith, split)
import Data.StrMap (StrMap(), fold, empty, keys, size, foldM, insert, lookup, values)
import Data.Tuple (Tuple(..), snd)
import Data.Traversable (traverse)

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (ST, STRef, readSTRef)

import Config
import System
import Util (replaceAll, lg)

type Shaders = {vert :: String, main :: String, disp :: String, aux :: Array String}
type CompRes = {component :: String, zOfs :: Int, parOfs :: Int, images :: Array String}

-- compile vertex, disp & main shaders
compileShaders :: forall eff h. Pattern -> (SystemST h) -> EpiS eff h Shaders
compileShaders pattern systemST = do
  vertRef <- loadLib pattern.vert systemST.moduleRefPool "compileShaders vert"
  vert    <- lift $ readSTRef vertRef
  vertRes <- compile vert systemST 0 0 []

  dispRef <- loadLib pattern.disp systemST.moduleRefPool "compileShaders disp"
  disp    <- lift $ readSTRef dispRef
  dispRes <- compile disp systemST 0 0 []

  mainRef <- loadLib pattern.main systemST.moduleRefPool "compileShaders main"
  main    <- lift $ readSTRef mainRef
  mainRes <- compile main systemST 0 0 []

  -- substitute includes
  includes <- traverse (\x -> loadLib x systemST.componentLib "includes") pattern.includes
  let allIncludes = "//INCLUDES\n" ++ (joinWith "\n\n" (map (\x -> x.body) includes)) ++ "\n//END INCLUDES\n"

  return {vert: vertRes.component, main: (allIncludes ++ mainRes.component), disp: (allIncludes ++ dispRes.component), aux: mainRes.images}


-- compile a shader.  submodules, par & zn
compile :: forall eff h. Module -> SystemST h -> Int -> Int -> (Array String) -> EpiS eff h CompRes
compile mod systemST zOfs parOfs images = do
  -- pars
  let k = (A.sort $ keys mod.par)
  let component'' = snd $ foldl handlePar (Tuple parOfs component') k
  let parOfs' = parOfs + (fromJust $ fromNumber $ size mod.par)

  -- zn
  let component''' = foldl handleZn component'' (A.(..) 0 ((A.length mod.zn) - 1))
  let zOfs' = zOfs + A.length mod.zn

  -- images
  let component'''' = foldl handleImg component''' (A.(..) 0 ((A.length mod.images) - 1))
  let images' = images ++ mod.images

  -- submodules
  mod <- loadModules mod.modules systemST.moduleRefPool
  foldM (handleChild systemST) { component: component'''', zOfs: zOfs', parOfs: parOfs', images: images' } mod
  where
    handlePar (Tuple n dt) v = Tuple (n + 1) (replaceAll ("@" ++ v ++ "@") ("par[" ++ show n ++ "]") dt)
    handleZn dt v = replaceAll ("zn\\[#" ++ show v ++ "\\]") ("zn[" ++ (show $ (v + zOfs)) ++ "]") dt
    handleImg dt v = replaceAll ("aux\\[#" ++ show v ++ "\\]") ("aux[" ++ (show $ (v + (A.length images))) ++ "]") dt
    handleChild systemST {component, zOfs, parOfs, images} k v = do
      res <- compile v systemST zOfs parOfs images
      let iC = "//" ++ k ++ "\n  {\n" ++ (indentLines 2 res.component) ++ "\n  }"
      let child = replaceAll ("%" ++ k ++ "%") iC component
      return $ res { component = child }


-- recursively flatten par & zn lists in a deterministic fashion
-- add own vars, then sort keys & recursively add
type LibParZn h = {lib :: StrMap (STRef h Module), par :: Array Number, zn :: Array Complex}
flattenParZn :: forall eff h. LibParZn h -> String -> EpiS eff h (LibParZn h)
flattenParZn {lib, par, zn} n = do
  mRef <- loadLib n lib "flattenParZn"
  mod <- lift $ readSTRef mRef
  let zn' = zn ++ mod.zn
  let par' = par ++ map (get mod.par) (A.sort $ keys mod.par)
  A.foldM flattenParZn {lib, par: par', zn: zn'} (fromList $ values mod.modules)
  where
    get dt n = fromJust $ (lookup n dt)


-- bulk load a list of modules
loadModules :: forall eff h. StrMap ModRef -> (StrMap (STRef h Module)) -> EpiS eff h (StrMap Module)
loadModules mr lib = do
  foldM handle empty mr
  where
    handle dt k v = do
      mRef <- loadLib v lib "loadModules"
      m    <- lift $ readSTRef mRef
      return $ insert k m dt


-- PRIVATE


spc :: Int -> String
spc 0 = ""
spc m = " " ++ spc (m - 1)


indentLines :: Int -> String -> String
indentLines n s = joinWith "\n" $ map (\x -> (spc n) ++ x) $ split "\n" s
