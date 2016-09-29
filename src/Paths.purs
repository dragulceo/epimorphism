module Paths where

import Prelude
import Config (Module, EpiS, Pattern, SystemST)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (modifySTRef, readSTRef, STRef)
import Control.Monad.Trans (lift)
import Data.Array (updateAt, uncons)
import Data.Complex (Cartesian(Cartesian), outCartesian, Polar(Polar), outPolar, Complex)
import Data.Maybe (Maybe(Just))
import Data.StrMap (StrMap, fold, empty, delete, insert, toList)
import Data.String (trim, split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Math (pi, min, cos, floor)
import System (loadLib, mSeq)
import Util (cxFromStringE, numFromStringE, intFromStringE, isNumber)

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

  {zn, par} <- return $ fold groupPaths {zn: empty, par: empty} m.paths

  traverse (runZnPath' mRef systemST.t) (toList zn)
  traverse (runParPath' mRef systemST.t) (toList par)
  return unit
  where
    runParPath' mRef t (Tuple idx path) = runParPath mRef t idx path
    groupPaths :: {zn :: (StrMap String), par :: (StrMap String)} -> String -> String -> {zn :: (StrMap String), par :: (StrMap String)}
    groupPaths {zn, par} k v = if (isNumber k) then {zn: insert k v zn, par} else {zn, par: insert k v par}

runZnPath' :: forall eff h. STRef h Module -> Number -> Tuple String String -> EpiS eff h Unit
runZnPath' mRef t (Tuple idx path) = do
  idx' <- intFromStringE idx
  runZnPath mRef t idx' path



runZnPath :: forall eff h. STRef h Module -> Number -> Int -> String -> EpiS eff h Unit
runZnPath mRef t idx pathStr = do
  Path func' conf {spd, args} <- parsePath pathStr
  spd' <- numFromStringE spd

  func <- case func' of
    PF2D f -> return f
    _ -> throwError "need a 2d path for zn"

  (Tuple val remove) <- func (t * spd') args

  m <- lift $ readSTRef mRef
  zn' <- case updateAt idx val m.zn of
    Just x -> return x
    _ -> throwError "idx out of bounds runZnPath"

  let paths' = if remove then (delete (show idx) m.paths) else m.paths

  lift $ modifySTRef mRef (\m' -> m' {zn = zn', paths = paths'})
  return unit

runParPath :: forall eff h. STRef h Module -> Number -> String -> String -> EpiS eff h Unit
runParPath mRef t var pathStr = do
  Path func' conf {spd, args} <- parsePath pathStr
  spd' <- numFromStringE spd

  func <- case func' of
    PF1D f -> return f
    _ -> throwError "need a 1d path for par"

  (Tuple val remove) <- func (t * spd') args

  m <- lift $ readSTRef mRef
  let par' = insert var val m.par
  let paths' = if remove then (delete var m.paths) else m.paths

  lift $ modifySTRef mRef (\m' -> m' {par = par', paths = paths'})
  return unit

type PathFunc1D eff h = Number -> (Array String) -> EpiS eff h (Tuple Number Boolean)
type PathFunc2D eff h = Number -> (Array String) -> EpiS eff h (Tuple Complex Boolean)
data PathFunc eff h = PF1D (PathFunc1D eff h) | PF2D (PathFunc2D eff h)

type PathArgs = {spd :: String, args :: Array String}
data PathConfig = PathConfig String
data Path eff h = Path (PathFunc eff h) PathConfig PathArgs

parsePath :: forall eff h. String -> EpiS eff h (Path eff h)
parsePath dta = do
  let dta' = split " " $ trim dta
  {head: name, tail: allargs} <- case uncons dta' of
    Just x -> return x
    _ -> throwError "invalid path syntax"

  {head: spd, tail: args} <- case uncons allargs of
    Just x -> return x
    _ -> throwError "first arg must be spd"

  Tuple func conf <- getPathObj name

  return $ Path func conf {spd, args}


getPathObj :: forall eff h. String -> EpiS eff h (Tuple (PathFunc eff h) PathConfig)
getPathObj name = do
  case name of
    "const1" -> return $ Tuple (PF1D const1D) (PathConfig "")
    "linear" -> return $ Tuple (PF1D linear1D) (PathConfig "")
    "loop"   -> return $ Tuple (PF1D loop1D) (PathConfig "")
    "smooth" -> return $ Tuple (PF1D smooth1D) (PathConfig "")
    "wave"   -> return $ Tuple (PF1D wave1D) (PathConfig "")
    "const2" -> return $ Tuple (PF2D const2D) (PathConfig "")
    "intrp"  -> return $ Tuple (PF2D intrp2D) (PathConfig "")
    "linx"   -> return $ Tuple (PF2D linx2D) (PathConfig "")
    "liny"   -> return $ Tuple (PF2D liny2D) (PathConfig "")
    "circle" -> return $ Tuple (PF2D circle2D) (PathConfig "")
    "rose"   -> return $ Tuple (PF2D rose2D) (PathConfig "")
    -- ""   -> return $ Tuple (PFD ) (PathConfig "")
    _ -> throwError $ "unknown path: " ++ name



-- 1D FUNCTIONS
const1D :: forall eff h. PathFunc1D eff h
const1D t args = do
  n <- case args of
    [x] -> return x
    _ -> throwError "invalid arguments for circle2D"

  n' <- numFromStringE n

  return $ Tuple n' false


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
    [a', b'] -> do
      a <- numFromStringE a'
      b <- numFromStringE b'
      return $ a * cos(2.0 * pi * t) + b
    _ -> throwError "invalid arguments for wave1D"
  return $ Tuple x false


-- 2D FUNCTIONS
const2D :: forall eff h. PathFunc2D eff h
const2D t args = do
  z <- case args of
    [x] -> return x
    _ -> throwError "invalid arguments for circle2D"

  z' <- cxFromStringE z

  return $ Tuple z' false


intrp2D :: forall eff h. PathFunc2D eff h
intrp2D t args = do
  z <- case args of
    [fromR', fromTh', toR', toTh'] -> do
      fromR <- numFromStringE fromR'
      fromTh <- numFromStringE fromTh'
      toR <- numFromStringE toR'
      toTh <- numFromStringE toTh'
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
    [r'] -> do
      r <- numFromStringE r'
      return $ outPolar $ Polar (2.0 * pi * t) r
    _ -> throwError "invalid arguments for circle2D"

  return $ Tuple z false


rose2D :: forall eff h. PathFunc2D eff h
rose2D t args = do
  z <- case args of
    [a', b', c'] -> do
      a <- numFromStringE a'
      b <- numFromStringE b'
      c <- numFromStringE c'
      return $ outPolar $ Polar (2.0 * pi * t) (a * cos(b * t) + c)
    _ -> throwError "invalid arguments for rose2D"

  return $ Tuple z false
