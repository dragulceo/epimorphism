module Scripts where

import Prelude
import Control.Monad.Except.Trans (throwError, lift)
import Control.Monad.ST (readSTRef)
import Data.Array (updateAt, index)
import Data.Complex (Cartesian(..), outCartesian, inPolar, Polar(Polar))
import Data.Library (dat, getLib, modLibD)
import Data.Maybe (fromMaybe, Maybe(Nothing, Just))
import Data.Script (ScriptRes(ScriptRes), ScriptFn)
import Data.Set (empty)
import Data.StrMap (insert, member)
import Data.Tuple (Tuple(..))
import Data.Types (Module)
import Math (max, round) as M
import ScriptUtil (addScript, purgeScript)
import System (loadLib)
import Text.Format (format, precision)
import Util (clickPause, cxFromString, inj, intFromStringE, log, numFromStringE)

null :: forall eff h. ScriptFn eff h
null ssRef lib t mid self dt = do
  pure $ ScriptRes empty Nothing

-- get rid of this abomination
pause :: forall eff h. ScriptFn eff h
pause ssRef lib t mid self dt = do
  lift $ clickPause
  purgeScript lib mid self
  pure $ ScriptRes empty Nothing

-- increment Zn
incZn :: forall eff h. ScriptFn eff h
incZn ssRef lib t mid self dt = do
  systemST <- lift $ readSTRef ssRef

  mod <- getLib lib mid "incZn module"
  let modD = dat (mod :: Module)

  idx <- (loadLib "idx" dt "incZn idx") >>= intFromStringE
  ofs <-  loadLib "ofs" dt "incZn ofs"

  case (index modD.zn idx) of
    Nothing -> throwError "index out of bounds - incZn"
    Just z' -> case cxFromString z' of
      Nothing -> pure unit
      Just (Tuple r i) -> do
        (Polar fromTh fromR) <- pure $ inPolar $ outCartesian (Cartesian r i)

        let incR = 0.1
        let incTh = 3.1415926535 / 4.0

        (Polar toTh toR) <- case ofs of
          "1" -> do
            let new = M.max ((M.round (fromR / incR + 1.0)) * incR) 0.0
            pure $ (Polar fromTh new)
          "-1" -> do
            let new = M.max ((M.round (fromR / incR - 1.0)) * incR) 0.0
            pure $ (Polar fromTh new)
          "i" -> do
            let new = (M.round (fromTh / incTh + 1.0)) * incTh
            pure $ (Polar new fromR)
          "-i" -> do
            let new = (M.round (fromTh / incTh - 1.0)) * incTh
            pure $ (Polar new fromR)
          _ -> throwError "offset should be +-1 or +-i"

        let path = inj "intrp@%0 4.0 %1 %2 %3 %4" [(format (precision 3) systemST.t), (format (precision 3) fromR), (format (precision 3) fromTh), (format (precision 3) toR), (format (precision 3) toTh)]

        let zn' = fromMaybe modD.zn (updateAt idx path modD.zn)
        modLibD lib mod _ {zn = zn'}

  -- remove self
  purgeScript lib mid self

  pure $ ScriptRes empty Nothing


randomize :: forall eff h. ScriptFn eff h
randomize ssRef lib t mid self dt = do
  systemST <- lift $ readSTRef ssRef

  dly  <- (loadLib "dly" dt "randomComponent") >>= numFromStringE
  spd  <-  loadLib "spd" dt "randomComponent"
  lib' <-  loadLib "lib" dt "randomComponent"
  sub  <-  loadLib "sub" dt "randomComponent"
  typ  <-  loadLib "typ" dt "randomComponent"

  nxt <- case (member "nxt" dt) of
    false -> pure t
    true  -> (loadLib "nxt" dt "randomize nxt") >>= numFromStringE

  -- next iteration
  update <- case t of
    t' | t' >= nxt -> do
      lift $ log "ITERATE COMPONENT"
      args <- pure $ case typ of
        "mod" -> inj "childN:%0 op:load by:query typ:mod query:%1 accs:rand spd:%2" [sub, lib', spd]
        _     -> inj "mut:%0 idx:%1 op:clone by:query typ:idx query:%2 accs:rand spd:%3" [typ, sub, lib', spd]

      addScript lib systemST.t mid "switch" args

      let nxt' = (format (precision 2) (t + dly))
      let dt' = insert "nxt" nxt' dt
      pure $ Just dt'
    _ -> pure Nothing

  pure $ ScriptRes empty update
