module Scripts where

import Prelude
import Config (ScriptFn)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (modifySTRef, readSTRef)
import Data.StrMap (insert, member)
import Pattern (purgeScript)
import ScriptUtil (parseAndImportScript)
import System (loadLib)
import Util (inj, numFromStringE, clickPause)

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
