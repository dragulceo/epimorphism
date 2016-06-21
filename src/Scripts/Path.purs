module Path where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (modifySTRef, readSTRef)
import Data.Array (updateAt) as A
import Data.Complex (outCartesian, outPolar, Polar(Polar), Cartesian(Cartesian))
import Data.Maybe (Maybe(Just))
import Data.StrMap (insert)
import Math (cos, floor, pi)

import Config (ScriptFn)
import System (loadLib)
import Util (numFromStringE, intFromStringE)

-- fixed point
pfix :: forall eff h. ScriptFn eff h
pfix ssRef pRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef
  let dt = scr.dt
  par <- loadLib "par" dt "pfix par"
  val <- (loadLib "val" dt "pfix val") >>= numFromStringE
  mRef <- loadLib mid systemST.moduleRefPool "pfix module"
  m <- lift $ readSTRef mRef

  let par' = insert par val m.par
  lift $ modifySTRef mRef (\m' -> m' {par = par'})

  return false

-- move par[par] around on a path
ppath :: forall eff h. ScriptFn eff h
ppath ssRef pRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef
  let dt = scr.dt

  -- get data
  spd <- (loadLib "spd" dt "ppath spd") >>= numFromStringE
  par <- loadLib "par" dt "ppath par"
  pathN <- loadLib "path" dt "ppath path"
  mRef <- loadLib mid systemST.moduleRefPool "ppath module"
  m <- lift $ readSTRef mRef

  -- lookup path function
  fn <- case pathN of
    "linear" -> do
      return $ \t -> t
    "loop" -> do
      return $ \t -> t - floor(t)
    "wave" -> do
      a <- (loadLib "a" dt "ppath linear a") >>= numFromStringE
      b <- (loadLib "b" dt "ppath linear b") >>= numFromStringE
      return $ \t -> a * cos(t) + b
    _ -> throwError $ "Unknown par path : " ++ pathN

  -- execute
  let val = fn (t * spd)

  -- modify data
  let par' = insert par val m.par
  lift $ modifySTRef mRef (\m' -> m' {par = par'})

  return false


-- fixed z
zfix :: forall eff h. ScriptFn eff h
zfix ssRef pRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef
  let dt = scr.dt
  idx <- (loadLib "idx" dt "zfix idx") >>= intFromStringE
  x <- (loadLib "x" dt "zfix x") >>= numFromStringE
  y <- (loadLib "y" dt "zfix y") >>= numFromStringE
  mRef <- loadLib mid systemST.moduleRefPool "zfix module"
  m <- lift $ readSTRef mRef

  let z = outCartesian $ Cartesian x y
  case (A.updateAt idx z m.zn) of
    (Just zn') -> lift $ modifySTRef mRef (\m' -> m' {zn = zn'})
    _ -> throwError $ "zn idx out of bound : " ++ (show idx) ++ " : in zfix"

  return false


-- move zn[idx] around on a path
zpath :: forall eff h. ScriptFn eff h
zpath ssRef pRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  scr <- lift $ readSTRef sRef
  let dt = scr.dt

  -- get data
  spd <- (loadLib "spd" dt "zpath spd") >>= numFromStringE
  idx <- (loadLib "idx" dt "zpath idx") >>= intFromStringE
  pathN <- loadLib "path" dt "zpath path"
  mRef <- loadLib mid systemST.moduleRefPool "zpath module"
  m <- lift $ readSTRef mRef

  -- lookup path function
  fn <- case pathN of
    "linx" -> return $ \t ->
      outCartesian $ Cartesian t 0.0
    "liny" -> return $ \t ->
      outCartesian $ Cartesian 0.0 t
    "circle" -> do
      r <- (loadLib "r" dt "zpath circle r") >>= numFromStringE
      return $ \t ->
        outPolar $ Polar (2.0 * pi * t) r
    "rose" -> do
      a <- (loadLib "a" dt "zpath rose a") >>= numFromStringE
      b <- (loadLib "b" dt "zpath rose b") >>= numFromStringE
      c <- (loadLib "c" dt "zpath rose c") >>= numFromStringE
      return $ \t ->
        outPolar $ Polar (2.0 * pi * t) (a * cos(b * t) + c)
    _ -> throwError $ "Unknown z path : " ++ pathN

  -- execute
  let z' = fn (t * spd)

  -- modify data
  case (A.updateAt idx z' m.zn) of
    (Just zn') -> lift $ modifySTRef mRef (\m' -> m' {zn = zn'})
    _ -> throwError $ "zn idx out of bound : " ++ (show idx) ++ " : in zpath"

  return false
