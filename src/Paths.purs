module Paths where

import Prelude
import Config (Module, EpiS, Pattern, SystemST)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (modifySTRef, readSTRef, STRef)
import Control.Monad.Trans (lift)
import Data.Array (updateAt, uncons)
import Data.Complex (Cartesian(Cartesian), outCartesian, Polar(Polar), outPolar, Complex)
import Data.Maybe (Maybe(Just))
import Data.StrMap (delete, insert, toList)
import Data.String (trim, split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Math (pi, min, cos, floor)
import System (loadLib, mSeq)
import Util (numFromStringE, intFromStringE)

runPaths :: forall eff h. STRef h (SystemST h) -> STRef h Pattern -> EpiS eff h Unit
runPaths ssRef pRef = do
  pattern  <- lift $ readSTRef pRef
  systemST <- lift $ readSTRef ssRef

  mSeq ssRef (runModPaths systemST) pattern.main
  mSeq ssRef (runModPaths systemST) pattern.disp
  mSeq ssRef (runModPaths systemST) pattern.vert

  return unit

runModPaths :: forall eff h. SystemST h -> String -> EpiS eff h Unit
runModPaths systemST mid = do
  mRef <- loadLib mid systemST.moduleRefPool "mid! runZnPaths"
  m <- lift $ readSTRef mRef

  traverse (runZnPath' mRef systemST.t) (toList m.znPaths)
  traverse (runParPath' mRef systemST.t) (toList m.parPaths)
  return unit
  where
    runParPath' mRef t (Tuple idx path) = runParPath mRef t idx path

runZnPath' :: forall eff h. STRef h Module -> Number -> Tuple String String -> EpiS eff h Unit
runZnPath' mRef t (Tuple idx path) = do
  idx' <- intFromStringE idx
  runZnPath mRef t idx' path



runZnPath :: forall eff h. STRef h Module -> Number -> Int -> String -> EpiS eff h Unit
runZnPath mRef t idx pathStr = do
  Path func' conf {spd, args} <- parsePath pathStr

  func <- case func' of
    PF2D f -> return f
    _ -> throwError "need a 2d path for zn"

  (Tuple val remove) <- func (t * spd) args

  m <- lift $ readSTRef mRef
  zn' <- case updateAt idx val m.zn of
    Just x -> return x
    _ -> throwError "idx out of bounds runZnPath"

  let znPaths' = if remove then (delete (show idx) m.znPaths) else m.znPaths

  lift $ modifySTRef mRef (\m' -> m' {zn = zn', znPaths = znPaths'})
  return unit

runParPath :: forall eff h. STRef h Module -> Number -> String -> String -> EpiS eff h Unit
runParPath mRef t var pathStr = do
  Path func' conf {spd, args} <- parsePath pathStr

  func <- case func' of
    PF1D f -> return f
    _ -> throwError "need a 1d path for par"

  (Tuple val remove) <- func (t * spd) args

  m <- lift $ readSTRef mRef
  let par' = insert var val m.par
  let parPaths' = if remove then (delete var m.parPaths) else m.parPaths

  lift $ modifySTRef mRef (\m' -> m' {par = par', parPaths = parPaths'})
  return unit

type PathFunc1D eff h = Number -> (Array Number) -> EpiS eff h (Tuple Number Boolean)
type PathFunc2D eff h = Number -> (Array Number) -> EpiS eff h (Tuple Complex Boolean)
data PathFunc eff h = PF1D (PathFunc1D eff h) | PF2D (PathFunc2D eff h)

type PathArgs = {spd :: Number, args :: Array Number}
data PathConfig = PathConfig String
data Path eff h = Path (PathFunc eff h) PathConfig PathArgs

parsePath :: forall eff h. String -> EpiS eff h (Path eff h)
parsePath dta = do
  let dta' = split " " $ trim dta
  Tuple name allargs <- case uncons dta' of
    Just { head: n, tail: rst } -> do
      args' <- traverse numFromStringE rst
      return $ Tuple n args'
    _ -> throwError "invalid path syntax"

  {head: spd, tail: args} <- case uncons allargs of
    Just x -> return x
    _ -> throwError "first arg must be spd"

  Tuple func conf <- getPathObj name

  return $ Path func conf {spd, args}


getPathObj :: forall eff h. String -> EpiS eff h (Tuple (PathFunc eff h) PathConfig)
getPathObj name = do
  case name of
    "linear" -> return $ Tuple (PF1D linear1D) (PathConfig "")
    "loop"   -> return $ Tuple (PF1D loop1D) (PathConfig "")
    "smooth" -> return $ Tuple (PF1D smooth1D) (PathConfig "")
    "wave"   -> return $ Tuple (PF1D wave1D) (PathConfig "")
    "intrp"  -> return $ Tuple (PF2D intrp2D) (PathConfig "")
    "linx"   -> return $ Tuple (PF2D linx2D) (PathConfig "")
    "liny"   -> return $ Tuple (PF2D liny2D) (PathConfig "")
    "circle" -> return $ Tuple (PF2D circle2D) (PathConfig "")
    "rose"   -> return $ Tuple (PF2D rose2D) (PathConfig "")
    -- ""   -> return $ Tuple (PFD ) (PathConfig "")
    _ -> throwError $ "unknown path: " ++ name



-- 1D FUNCTIONS
linear1D :: forall eff h. PathFunc1D eff h
linear1D t args = do
  let x = t
  return $ Tuple x false

loop1D :: forall eff h. PathFunc1D eff h
loop1D t args = do
  let x = (t - floor(t))
  return $ Tuple x false

smooth1D :: forall eff h. PathFunc1D eff h
smooth1D t args = do
  let x = t * t * (3.0 - 2.0 * t)
  return $ Tuple x false

wave1D :: forall eff h. PathFunc1D eff h
wave1D t args = do
  x <- case args of
    [a, b] -> return $ a * cos(t) + b
    _ -> throwError "invalid arguments for wave1D"
  return $ Tuple x false


-- 2D FUNCTIONS
intrp2D :: forall eff h. PathFunc2D eff h
intrp2D t args = do
  z <- case args of
    [fromR, fromTh, toR, toTh] -> do
      let t' = min t 1.0
      let r = toTh * t' + fromTh * (1.0 - t')
      let th = toR * t' + fromR * (1.0 - t')
      return $ outPolar $ Polar r th
    _ -> throwError "invalid arguments for intrp2D"

  return $ Tuple z (t >= 1.0)


linx2D :: forall eff h. PathFunc2D eff h
linx2D t args = do
  let z = outCartesian $ Cartesian t 0.0
  return $ Tuple z false


liny2D :: forall eff h. PathFunc2D eff h
liny2D t args = do
  let z = outCartesian $ Cartesian 0.0 t
  return $ Tuple z false


circle2D :: forall eff h. PathFunc2D eff h
circle2D t args = do
  z <- case args of
    [r] ->
      return $ outPolar $ Polar (2.0 * pi * t) r
    _ -> throwError "invalid arguments for circle2D"

  return $ Tuple z false


rose2D :: forall eff h. PathFunc2D eff h
rose2D t args = do
  z <- case args of
    [a, b, c] -> do
      return $ outPolar $ Polar (2.0 * pi * t) (a * cos(b * t) + c)
    _ -> throwError "invalid arguments for rose2D"

  return $ Tuple z false
