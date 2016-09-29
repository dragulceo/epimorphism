module Paths where

import Prelude
import Config (Module, EpiS, Pattern, SystemST)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (modifySTRef, readSTRef, STRef)
import Control.Monad.Trans (lift)
import Data.Array (length, updateAt, uncons)
import Data.Complex (Complex, Cartesian(Cartesian), outCartesian, Polar(Polar), outPolar)
import Data.Maybe (Maybe(Just))
import Data.StrMap (fromFoldable, foldM, StrMap, fold, empty, delete, insert, toList)
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

  func <- case func' of
    PF2D f -> return f
    _ -> throwError "need a 2d path for zn"

  (Tuple val remove) <- func (t * spd) args

  m <- lift $ readSTRef mRef
  zn' <- case updateAt idx val m.zn of
    Just x -> return x
    _ -> throwError "idx out of bounds runZnPath"

  let paths' = if remove then (delete (show idx) m.paths) else m.paths

  lift $ modifySTRef mRef (\m' -> m' {zn = zn', paths = paths'})
  return unit

runParPath :: forall eff h. STRef h Module -> Number -> String -> String -> EpiS eff h Unit
runParPath mRef t var pathStr = do
  Path func' conf args <- parsePath pathStr

  ArgN spd <- loadLib "spd" args "need speed in path"

  func <- case func' of
    PF1D f -> return f
    _ -> throwError "need a 1d path for par"

  (Tuple val remove) <- func (t * spd) args

  m <- lift $ readSTRef mRef
  let par' = insert var val m.par
  let paths' = if remove then (delete var m.paths) else m.paths

  lift $ modifySTRef mRef (\m' -> m' {par = par', paths = paths'})
  return unit

----------------- PARSING -----------------

type PathFunc1D eff h = Number -> PathArgs -> EpiS eff h (Tuple Number Boolean)
type PathFunc2D eff h = Number -> PathArgs -> EpiS eff h (Tuple Complex Boolean)
data PathFunc eff h = PF1D (PathFunc1D eff h) | PF2D (PathFunc2D eff h)

data PathArg = ArgS String | ArgI Int | ArgN Number | ArgN Complex
type PathArgs = StrMap PathArg

data ArgType = ArgTS | ArgTI | ArgTN | ArgTC
data ArgInfo = ArgInfo String ArgType String -- name, info, default
data PathConfig = PathConfig String (Array ArgInfo)

data Path eff h = Path (PathFunc eff h) PathConfig PathArgs

parsePath :: forall eff h. String -> EpiS eff h (Path eff h)
parsePath dta = do
  let dta' = split " " $ trim dta

  {head: name, allargs: rst} <- case uncons dta' of
    Just x -> return x
    _ -> throwError "invalid path syntax"

  Tuple func (conf@(PathConfig _ argsconf)) <- getPathObj name

  when (length allargs /= length argsconf) do
    throwError $ "wrong number of args for: " ++ name

  argsT <- traverse parseArg $ zip args argsconf
  let args = fromFoldable argsT

  return $ Path func conf args

  where
    parseArg (Tuple val (ArgInfo name ArgTS _)) = do return $ Tuple name (ArgS val)
    parseArg (Tuple val (ArgInfo name ArgTI _)) = do
      val' <- intFromStringE val
      return $ Tuple name (ArgI val')
    parseArg (Tuple val (ArgInfo name ArgTN _)) = do
      val' <- numFromStringE val
      return $ Tuple name (ArgN val')
    parseArg (Tuple val (ArgInfo name ArgTC _)) = do
      val' <- cxFromStringE val
      return $ Tuple name (ArgC val')


getPathObj :: forall eff h. String -> EpiS eff h (Tuple (PathFunc eff h) PathConfig)
getPathObj name = do
  case name of
    "linear" -> return $ Tuple (PF1D linear1D) (PathConfig "" [ArgInfo "spd" ArgTN "1.0"])
    "loop"   -> return $ Tuple (PF1D loop1D) (PathConfig "" [ArgInfo "spd" ArgTN "1.0"])
    "smooth" -> return $ Tuple (PF1D smooth1D) (PathConfig "" [ArgInfo "spd" ArgTN "1.0"])
    "wave"   -> return $ Tuple (PF1D wave1D) (PathConfig "" [ArgInfo "spd" ArgTN "1.0", ArgInfo "a" ArgTN "1.0", ArgInfo "b" ArgTN "0.0"])
    "intrp"  -> return $ Tuple (PF2D intrp2D) (PathConfig "" [ArgInfo "spd" ArgTN "1.0",
                                                              ArgInfo "fromR" ArgTN "1.0", ArgInfo "fromTh" ArgTN "0.0",
                                                              ArgInfo "toR" ArgTN "1.0", ArgInfo "toTh" ArgTN "0.0"])
    "linx"   -> return $ Tuple (PF2D linx2D) (PathConfig "" [ArgInfo "spd" ArgTN "1.0"])
    "liny"   -> return $ Tuple (PF2D liny2D) (PathConfig "" [ArgInfo "spd" ArgTN "1.0"])
    "circle" -> return $ Tuple (PF2D circle2D) (PathConfig "" [ArgInfo "spd" ArgTN "1.0", ArgInfo "r" ArgTN "1.0"])
    "rose"   -> return $ Tuple (PF2D rose2D) (PathConfig "" [ArgInfo "spd" ArgTN "1.0", ArgInfo "a" ArgTN "1.0", ArgInfo "b" ArgTN "1.0", ArgInfo "c" ArgTN "0.0"])
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
    [a, b] -> return $ a * cos(2.0 * pi * t) + b
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
