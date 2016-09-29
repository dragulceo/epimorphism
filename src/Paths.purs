module Paths where

import Prelude
import Config (Module, EpiS, Pattern, SystemST)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (modifySTRef, readSTRef, STRef)
import Control.Monad.Trans (lift)
import Data.Array (updateAt, uncons)
import Data.Complex (inCartesian, Cartesian(Cartesian), outCartesian, Polar(Polar), outPolar, Complex)
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
    runParPath' mRef t (Tuple idx path) = runPath true mRef t idx path
    runZnPath' mRef t (Tuple idx path)  = runPath false mRef t idx path
    groupPaths :: {zn :: (StrMap String), par :: (StrMap String)} -> String -> String -> {zn :: (StrMap String), par :: (StrMap String)}
    groupPaths {zn, par} k v = if (isNumber k) then {zn: insert k v zn, par} else {zn, par: insert k v par}


runPath :: forall eff h. Boolean -> STRef h Module -> Number -> String -> String -> EpiS eff h Unit
runPath isPar mRef t idx pathStr = do
  Path func conf {spd, args} <- parsePath pathStr
  spd' <- numFromStringE spd

  (Tuple val remove) <- func (t * spd') args

  m <- lift $ readSTRef mRef

  case isPar of
    true -> do
      Cartesian x y <- return $ inCartesian val

      let par' = insert idx x m.par
      let paths' = if remove then (delete idx m.paths) else m.paths

      lift $ modifySTRef mRef (\m' -> m' {par = par', paths = paths'})
      return unit
    false -> do
      idx' <- intFromStringE idx
      zn' <- case updateAt idx' val m.zn of
        Just x -> return x
        _ -> throwError "idx out of bounds runZnPath"

      let paths' = if remove then (delete (show idx) m.paths) else m.paths

      lift $ modifySTRef mRef (\m' -> m' {zn = zn', paths = paths'})
      return unit


------------------------------ PARSING ------------------------------

type PathFunc eff h = Number -> (Array String) -> EpiS eff h (Tuple Complex Boolean)

type PathArgs = {spd :: String, args :: Array String}
data PathConfig = PathConfig String
data Path eff h = Path (PathFunc eff h) PathConfig PathArgs

parsePath :: forall eff h. String -> EpiS eff h (Path eff h)
parsePath dta = do
  let dta' = split " " $ trim dta

  {head: name, tail: allargs} <- case dta' of
    [x] -> do
      return {head: "const", tail: ["0.0", x]}
    _ -> do
      case uncons dta' of
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
    "const" -> return $ Tuple cnst (PathConfig "")
    "linear" -> return $ Tuple linear (PathConfig "")
    "loop"   -> return $ Tuple loop (PathConfig "")
    "smooth" -> return $ Tuple smooth (PathConfig "")
    "wave"   -> return $ Tuple wave (PathConfig "")
    "intrp"  -> return $ Tuple intrp (PathConfig "")
    "linx"   -> return $ Tuple linx (PathConfig "")
    "liny"   -> return $ Tuple liny (PathConfig "")
    "circle" -> return $ Tuple circle (PathConfig "")
    "rose"   -> return $ Tuple rose (PathConfig "")
    -- ""   -> return $ Tuple (PFD ) (PathConfig "")
    _ -> throwError $ "unknown path: " ++ name



-- 1D FUNCTIONS
cnst :: forall eff h. PathFunc eff h
cnst t args = do
  z <- case args of
    [x] -> return x
    _ -> throwError "invalid arguments for const"

  z' <- cxFromStringE z

  return $ Tuple z' false


linear :: forall eff h. PathFunc eff h
linear t args = do
  let z = outCartesian (Cartesian t 0.0)
  return $ Tuple z false

loop :: forall eff h. PathFunc eff h
loop t args = do
  let x = (t - floor(t))
  let z = outCartesian (Cartesian x 0.0)
  return $ Tuple z false

smooth :: forall eff h. PathFunc eff h
smooth t args = do
  let x = t * t * (3.0 - 2.0 * t)
  let z = outCartesian (Cartesian x 0.0)
  return $ Tuple z false

wave :: forall eff h. PathFunc eff h
wave t args = do
  x <- case args of
    [a', b'] -> do
      a <- numFromStringE a'
      b <- numFromStringE b'
      return $ a * cos(2.0 * pi * t) + b
    _ -> throwError "invalid arguments for wave"
  let z = outCartesian (Cartesian x 0.0)
  return $ Tuple z false


--  FUNCTIONS
intrp :: forall eff h. PathFunc eff h
intrp t args = do
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
    _ -> throwError "invalid arguments for intrp"

  return $ Tuple z (t >= 1.0)


linx :: forall eff h. PathFunc eff h
linx t args = do
  let z = outCartesian $ Cartesian t 0.0
  return $ Tuple z false


liny :: forall eff h. PathFunc eff h
liny t args = do
  let z = outCartesian $ Cartesian 0.0 t
  return $ Tuple z false


circle :: forall eff h. PathFunc eff h
circle t args = do
  z <- case args of
    [r'] -> do
      r <- numFromStringE r'
      return $ outPolar $ Polar (2.0 * pi * t) r
    _ -> throwError "invalid arguments for circle"

  return $ Tuple z false


rose :: forall eff h. PathFunc eff h
rose t args = do
  z <- case args of
    [a', b', c'] -> do
      a <- numFromStringE a'
      b <- numFromStringE b'
      c <- numFromStringE c'
      return $ outPolar $ Polar (2.0 * pi * t) (a * cos(b * t) + c)
    _ -> throwError "invalid arguments for rose"

  return $ Tuple z false
