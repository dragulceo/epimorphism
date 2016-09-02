module Paths where

import Prelude
import Config (Module, EpiS, Pattern, SystemST)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (modifySTRef, readSTRef, STRef)
import Control.Monad.Trans (lift)
import Data.Array (updateAt, uncons)
import Data.Complex (Complex)
import Data.Maybe (Maybe(Just))
import Data.StrMap (delete, insert, toList)
import Data.String (split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
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
  PathAll func' conf {spd, args} <- parsePath pathStr

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
  PathAll func' conf {spd, args} <- parsePath pathStr

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
data PathAll eff h = PathAll (PathFunc eff h) PathConfig PathArgs

parsePath :: forall eff h. String -> EpiS eff h (PathAll eff h)
parsePath dta = do
  let dta' = split " " dta
  Tuple name allargs <- case uncons dta' of
    Just { head: n, tail: rst } -> do
      args' <- traverse numFromStringE rst
      return $ Tuple n args'
    _ -> throwError "invalid path syntax"

  {head: spd, tail: args} <- case uncons allargs of
    Just x -> return x
    _ -> throwError "first arg must be spd"

  Tuple func conf <- getPathObj name

  return $ PathAll func conf {spd, args}


getPathObj :: forall eff h. String -> EpiS eff h (Tuple (PathFunc eff h) PathConfig)
getPathObj name = do
  case name of
    "linear" -> return $ Tuple (PF1D linear1D) (PathConfig "")
    _ -> throwError $ "unknown path: " ++ name


linear1D :: forall eff h. PathFunc1D eff h
linear1D t args = do
  return $ Tuple 1.0 false
