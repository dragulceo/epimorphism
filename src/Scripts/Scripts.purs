module Scripts where

import Prelude
import Config (PMut(PMutNone), ScriptRes(ScriptRes), ScriptFn)
import Control.Monad.Except.Trans (throwError, lift)
import Control.Monad.ST (modifySTRef, readSTRef)
import Data.Array (updateAt, index)
import Data.Complex (inPolar, Polar(Polar))
import Data.Maybe (fromMaybe, Maybe(Nothing, Just))
import Data.StrMap (insert, member)
import Math (max, round)
import ScriptUtil (addScript, purgeScript)
import System (loadLib)
import Text.Format (format, precision)
import Util (cxFromStringE, intFromStringE, inj, numFromStringE, clickPause)

null :: forall eff h. ScriptFn eff h
null ssRef pRef t mid idx dt = do
  pure $ ScriptRes PMutNone Nothing

-- get rid of this abomination
pause :: forall eff h. ScriptFn eff h
pause ssRef pRef t mid idx dt = do
  systemST <- lift $ readSTRef ssRef

  lift $ clickPause
  purgeScript systemST mid idx
  pure $ ScriptRes PMutNone Nothing

-- increment Zn
incZn :: forall eff h. ScriptFn eff h
incZn ssRef pRef t mid idx dt = do
  systemST <- lift $ readSTRef ssRef

  mRef <- loadLib mid systemST.moduleRefPool "incZn module"
  mod  <- lift $ readSTRef mRef

  idx <- (loadLib "idx" dt "incZn idx") >>= intFromStringE
  ofs <-  loadLib "ofs" dt "incZn ofs"

  z <- case (index mod.zn idx) of
    Just z' -> cxFromStringE z'
    _ -> throwError "index out of bounds - incZn"

  (Polar fromTh fromR) <- pure $ inPolar z

  let incR = 0.1
  let incTh = 3.1415926535 / 4.0

  (Polar toTh toR) <- case ofs of
    "1" -> do
      let new = max ((round (fromR / incR + 1.0)) * incR) 0.0
      pure $ (Polar fromTh new)
    "-1" -> do
      let new = max ((round (fromR / incR - 1.0)) * incR) 0.0
      pure $ (Polar fromTh new)
    "i" -> do
      let new = (round (fromTh / incTh + 1.0)) * incTh
      pure $ (Polar new fromR)
    "-i" -> do
      let new = (round (fromTh / incTh - 1.0)) * incTh
      pure $ (Polar new fromR)
    _ -> throwError "offset should be +-1 or +-i"

  let tPhase = systemST.t - t -- recover phase
  let path = inj "intrp@%0 4.0 %1 %2 %3 %4" [(show $ t + tPhase), (show fromR), (show fromTh), (show toR), (show toTh)]

  let zn' = fromMaybe mod.zn (updateAt idx path mod.zn)
  lift $ modifySTRef mRef (\m -> m {zn = zn'})

  -- remove self
  purgeScript systemST mid idx

  pure $ ScriptRes PMutNone Nothing


randomize :: forall eff h. ScriptFn eff h
randomize ssRef pRef t mid idx dt = do
  systemST <- lift $ readSTRef ssRef

  dly <- (loadLib "dly" dt "randomComponent") >>= numFromStringE
  spd <-  loadLib "spd" dt "randomComponent"
  lib <-  loadLib "lib" dt "randomComponent"
  sub <-  loadLib "sub" dt "randomComponent"
  typ <-  loadLib "typ" dt "randomComponent"

  nxt <- case (member "nxt" dt) of
    false -> pure t
    true  -> (loadLib "nxt" dt "randomMain1 nxt") >>= numFromStringE

  -- next iteration
  update <- case t of
    t | t >= nxt -> do
      --let a = lg "ITERATE COMPONENT"
      args <- pure $ case typ of
        "mod" -> inj "childN:%0 op:load by:query typ:mod query:%1 accs:rand spd:%2" [sub, lib, spd]
        _     -> inj "mut:%0 idx:%1 op:clone by:query typ:idx query:%2 accs:rand spd:%3" [typ, sub, lib, spd]

      addScript systemST mid "switch" args

      let nxt' = (format (precision 2) (t + dly))
      let dt' = insert "nxt" nxt' dt
      pure $ Just dt'
    _ -> pure Nothing

  pure $ ScriptRes PMutNone update
