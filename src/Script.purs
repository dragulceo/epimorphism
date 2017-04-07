module Script where

import Prelude
import Config (PMut(PMut, PMutNone), Script(Script), ScriptRes(ScriptRes), SystemST, ScriptFn, EpiS)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (readSTRef, STRef)
import Control.Monad.Trans.Class (lift)
import Data.Array (foldM, elemIndex, updateAt)
import Data.Library (Library, getPatternD)
import Data.Maybe (Maybe(Just))
import Data.Set (union)
import ScriptUtil (serializeScript, parseScript)
import Scripts (null, randomize, pause, incZn)
import Switch (finishSwitch, switch)
import System (mFold, mUp, loadLib)
import Util (dbg, lg, fromJustE)

-- find script fuction given name
lookupScriptFN :: forall eff h. String -> EpiS eff h (ScriptFn eff h)
lookupScriptFN n = case n of
  "null"         -> pure null
  "switch"       -> pure switch
  "incZn"        -> pure incZn
  "finishSwitch" -> pure finishSwitch
  "pause"        -> pure pause
  "randomize"    -> pure randomize
  _              -> throwError $ "script function not found: " <> n


-- execute all scripts & script pool. abort as soon as something mutates the pattern
runScripts :: forall eff h. STRef h (SystemST h) -> Library h -> EpiS eff h PMut
runScripts ssRef lib = do
  patternD <- getPatternD lib "runScripts pattern"
  r0 <- mFold ssRef PMutNone patternD.main (runModScripts ssRef lib)
  r1 <- mFold ssRef r0 patternD.disp (runModScripts ssRef lib)
  mFold ssRef r1 patternD.vert (runModScripts ssRef lib)


runModScripts :: forall eff h. STRef h (SystemST h) -> Library h -> PMut -> String -> EpiS eff h PMut
runModScripts ssRef lib mut mid = do
  systemST <- lift $ readSTRef ssRef
  mRef     <- loadLib mid systemST.moduleRefPool "mid! runScripts"
  m        <- lift $ readSTRef mRef

  foldM (runScript ssRef lib mid) mut m.scripts


runScript :: forall eff h. STRef h (SystemST h) -> Library h -> String -> PMut -> String -> EpiS eff h PMut
runScript ssRef lib mid pmut scr = do
  --dbg $ "running script : " <> scr <> " : for " <> mid
  --dbg x
  (Script name phase args) <- parseScript scr
  systemST <- lift $ readSTRef ssRef
  fn <- lookupScriptFN name
  let t' = systemST.t - phase

  mRef <- loadLib mid systemST.moduleRefPool "mid! runScript"
  m    <- lift $ readSTRef mRef
  idx  <- fromJustE (elemIndex scr m.scripts) "script not found m"
  (ScriptRes mut update) <- fn ssRef lib t' mid idx args
  case update of
    Just dt -> do
      let new = serializeScript (Script name phase dt)
      m'        <- lift $ readSTRef mRef
      systemST' <- lift $ readSTRef ssRef
      idx'      <- fromJustE (elemIndex scr m'.scripts) "script not found m'"
      scripts'  <- fromJustE (updateAt idx' new m'.scripts) "should be safe runScript"

      mUp systemST' mid \m1 -> m1 {scripts = scripts'} -- not sure this was right
      pure unit
    _ -> pure unit

  case pmut of
    PMutNone -> pure mut
    PMut pat res -> do
      case mut of
        PMutNone -> pure pmut
        PMut mutP mutS -> pure $ PMut pat (union res mutS)

--runScript _ _ _ x _ = pure x
