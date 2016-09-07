module Compiler where

import System
import Config (Epi, Module, EpiS, ModRef, SystemST, Pattern)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (STRef, readSTRef)
import Data.Array (sort, length, foldM, (..)) as A
import Data.Complex (Complex)
import Data.Foldable (foldl)
import Data.Int (fromNumber)
import Data.List (fromList)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (lookup, StrMap, fold, empty, keys, size, foldM, insert, values)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import Prelude (return, ($), bind, map, (++), (-), (+), show)
import Util (lg, replaceAll, indentLines)

type Shaders = {vert :: String, main :: String, disp :: String, aux :: Array String}
type CompRes = {component :: String, zOfs :: Int, parOfs :: Int, images :: Array String}

foreign import parseT :: String -> String

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


-- compile a shader.  substitutions, submodules, par & zn
compile :: forall eff h. Module -> SystemST h -> Int -> Int -> (Array String) -> EpiS eff h CompRes
compile mod systemST zOfs parOfs images = do
  -- substitutions (make sure this is always first)
  comp <- loadLib mod.component systemST.componentLib "compile component"
  sub' <- preProcessSub mod.sub
  let component' = fold handleSub comp.body sub'

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
    handleSub dt k v = replaceAll ("\\$" ++ k ++ "\\$") v dt
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

-- preprocess substitutions.  just parses t expressions
preProcessSub :: forall eff. StrMap String -> Epi eff (StrMap String)
preProcessSub sub = do
  case (lookup "t_expr" sub) of
    (Just expr) -> do
      expr' <- parseTexp expr
      let sub' = insert "t_expr" expr' sub
      return sub'
    Nothing -> do
      return sub

parseTexp :: forall eff. String -> Epi eff String
parseTexp expr = do
  let expr1 = parseT expr

  let subs = [["\\+","A"],["\\-","S"],["\\~","CONJ"],["\\+","A"],["\\-","S"],["\\*","M"],["/","D"],["sinh","SINHZ"],
              ["cosh","COSHZ"],["tanh","TANHZ"],["sin","SINZ"],["cos","COSZ"],["tan","TANZ"],["exp","EXPZ"],["sq","SQZ"]]

  return $ foldl f expr1 subs
  where
    f exp [a, b] = replaceAll a b exp
    f exp _ = exp


-- Bulk load a list of modules
loadModules :: forall eff h. StrMap ModRef -> (StrMap (STRef h Module)) -> EpiS eff h (StrMap Module)
loadModules mr lib = do
  foldM handle empty mr
  where
    handle dt k v = do
      mRef <- loadLib v lib "loadModules"
      m    <- lift $ readSTRef mRef
      return $ insert k m dt
