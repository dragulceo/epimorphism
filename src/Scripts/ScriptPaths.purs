module ScriptPaths where

import Prelude
import Config (ScriptFn)
import Control.Monad (when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (modifySTRef, readSTRef)
import Data.Array (index)
import Data.Array (updateAt) as A
import Data.Complex (inPolar, outPolar, outCartesian, Polar(Polar), Cartesian(Cartesian))
import Data.Maybe (Maybe(Just))
import Data.StrMap (insert)
import Data.Tuple (Tuple(Tuple))
import Math (max, min, pi, round, cos, floor)
import Pattern (purgeScript)
import ScriptUtil (parseAndImportScript)
import System (loadLib)
import Util (inj, numFromStringE, intFromStringE)

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
    "smooth" -> do
      return $ \t -> t * t * (3.0 - 2.0 * t)
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
    "intrp" -> do
      fromR <- (loadLib "fromR" dt "intrp fromR") >>= numFromStringE
      fromTh <- (loadLib "fromTh" dt "intrp fromTh") >>= numFromStringE
      toR <- (loadLib "toR" dt "intrp toR") >>= numFromStringE
      toTh <- (loadLib "toTh" dt "intrp toTh") >>= numFromStringE

      return $ \t -> let t' = (min t 1.0) in
        Tuple (outPolar $ Polar (toTh * t' + fromTh * (1.0 - t')) (toR * t' + fromR * (1.0 - t'))) (t >= 1.0)
    "linx" -> return $ \t ->
      Tuple (outCartesian $ Cartesian t 0.0) false
    "liny" -> return $ \t ->
      Tuple (outCartesian $ Cartesian 0.0 t) false
    "circle" -> do
      r <- (loadLib "r" dt "zpath circle r") >>= numFromStringE
      return $ \t ->
        Tuple (outPolar $ Polar (2.0 * pi * t) r) false
    "rose" -> do
      a <- (loadLib "a" dt "zpath rose a") >>= numFromStringE
      b <- (loadLib "b" dt "zpath rose b") >>= numFromStringE
      c <- (loadLib "c" dt "zpath rose c") >>= numFromStringE
      return $ \t ->
        Tuple (outPolar $ Polar (2.0 * pi * t) (a * cos(b * t) + c)) false
    _ -> throwError $ "Unknown z path : " ++ pathN

  -- execute
  (Tuple z' remove) <- return $ fn (t * spd)

  -- remove self
  when remove do
    purgeScript ssRef mid self

  -- modify data
  case (A.updateAt idx z' m.zn) of
    (Just zn') -> lift $ modifySTRef mRef (\m' -> m' {zn = zn'})
    _ -> throwError $ "zn idx out of bound : " ++ (show idx) ++ " : in zpath"

  return false


-- increment Zn
incZn :: forall eff h. ScriptFn eff h
incZn ssRef pRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  pattern  <- lift $ readSTRef pRef

  scr <- lift $ readSTRef sRef
  let dt = scr.dt

  mRef <- loadLib mid systemST.moduleRefPool "incZn module"
  mod  <- lift $ readSTRef mRef

  idx <- (loadLib "idx" dt "incZn idx") >>= intFromStringE
  ofs <-  loadLib "ofs" dt "incZn ofs"

  z <- case (index mod.zn idx) of
    Just z' -> return z'
    _ -> throwError "index out of bounds - incZn"

  (Polar fromTh fromR) <- return $ inPolar z

  let incR = 0.1
  let incTh = 3.1415926535 / 4.0

  (Polar toTh toR) <- case ofs of
    "1" -> do
      let new = max ((round (fromR / incR + 1.0)) * incR) 0.0
      return $ (Polar fromTh new)
    "-1" -> do
      let new = max ((round (fromR / incR - 1.0)) * incR) 0.0
      return $ (Polar fromTh new)
    "i" -> do
      let new = (round (fromTh / incTh + 1.0)) * incTh
      return $ (Polar new fromR)
    "-i" -> do
      let new = (round (fromTh / incTh - 1.0)) * incTh
      return $ (Polar new fromR)
    _ -> throwError "offset should be +-1 or +-i"

  parseAndImportScript ssRef pattern mid $ inj "zpath path:intrp idx:%0 spd:4.0 fromTh:%1 toTh:%2 fromR:%3 toR:%4" [(show idx), (show fromTh), (show toTh), (show fromR), (show toR)]

  -- remove self
  purgeScript ssRef mid self

  return false
