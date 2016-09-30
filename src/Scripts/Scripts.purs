module Scripts where

import Prelude
import Config (ScriptFn)
import Control.Monad.Except.Trans (throwError, lift)
import Control.Monad.ST (modifySTRef, readSTRef)
import Data.Array (updateAt, index)
import Data.Complex (inPolar, Polar(Polar))
import Data.Maybe (fromMaybe, Maybe(Just))
import Data.StrMap (insert, member)
import Math (max, round)
import Pattern (purgeScript)
import ScriptUtil (parseAndImportScript)
import System (loadLib)
import Util (cxFromStringE, intFromStringE, inj, numFromStringE, clickPause)

-- get rid of this abomination
pause :: forall eff h. ScriptFn eff h
pause ssRef pRef self t rootId sRef = do
  lift $ clickPause
  purgeScript ssRef rootId self
  return false


randomize :: forall eff h. ScriptFn eff h
randomize ssRef pRef self t mid sRef = do
  systemST <- lift $ readSTRef ssRef
  pattern  <- lift $ readSTRef pRef
  scr      <- lift $ readSTRef sRef

  dly <- (loadLib "dly" scr.dt "randomComponent") >>= numFromStringE
  spd <-  loadLib "spd" scr.dt "randomComponent"
  lib <-  loadLib "lib" scr.dt "randomComponent"
  sub <-  loadLib "sub" scr.dt "randomComponent"
  typ <-  loadLib "typ" scr.dt "randomComponent"
  adr <-  loadLib "adr" scr.dt "randomComponent"

  nxt <- case (member "nxt" scr.dt) of
    false -> return t
    true  -> (loadLib "nxt" scr.dt "randomMain1 nxt") >>= numFromStringE

  -- next iteration
  case t of
    t | t >= nxt -> do
      --let a = lg "ITERATE COMPONENT"
      let dt' = insert "nxt" (show (t + dly)) scr.dt
      lift $ modifySTRef sRef (\s -> s {dt = dt'})
      adr' <- case (adr == "!") of
        true -> return mid
        false -> return adr

      case typ of
        "mod" -> do
          parseAndImportScript ssRef pattern adr' $ inj "switch childN:%0 op:load by:query typ:mod query:%1 accs:rand spd:%2" [sub, lib, spd]
        _ -> do
          parseAndImportScript ssRef pattern adr' $ inj "switch mut:%0 idx:%1 op:clone by:query typ:idx query:%2 accs:rand spd:%3" [typ, sub, lib, spd]

      return unit
    _ -> return unit

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
    Just z' -> cxFromStringE z'
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

  let path = inj "intrp@%0 4.0 %1 %2 %3 %4" [(show $ t + scr.tPhase), (show fromR), (show fromTh), (show toR), (show toTh)]

  let zn' = fromMaybe mod.zn (updateAt idx path mod.zn)
  lift $ modifySTRef mRef (\m -> m {zn = zn'})

  -- remove self
  purgeScript ssRef mid self

  return false
