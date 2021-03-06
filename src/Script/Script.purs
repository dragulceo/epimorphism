module Script.Script where

import Prelude
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (readSTRef, STRef)
import Control.Monad.Trans.Class (lift)
import Data.Array (foldM, elemIndex, updateAt)
import Data.Foldable (foldl)
import Data.Kernels (Kernel, kAcs)
import Data.Library (getLib, getPatternD, mD, modLibD)
import Data.Maybe (Maybe(Just))
import Data.Script (Script(..), ScriptFn, ScriptRes(ScriptRes))
import Data.Set (Set, empty)
import Data.System (SystemST)
import Data.Types (EpiS, Library, Module(..))
import Script.ScriptUtil (parseScript, serializeScript)
import Script.Scripts (null, randomize, pause, incZn)
import Script.Switch (finishSwitch, switch)
import System (mFold)
import Util (fromJustE, log)

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
runScripts :: forall eff h. STRef h (SystemST h) -> Library h -> EpiS eff h (Set Kernel)
runScripts ssRef lib = do
  patternD <- getPatternD lib "runScripts pattern"
  let kernels = kAcs <*> (pure patternD)
  let sfunc = \x y -> mFold lib y x (runModScripts ssRef lib)
  let mfolds = pure sfunc <*> kernels
  foldl (>>=) (pure empty) mfolds


runModScripts :: forall eff h. STRef h (SystemST h) -> Library h -> (Set Kernel) -> String -> EpiS eff h (Set Kernel)
runModScripts ssRef lib kernels mid = do
  modD <- mD <$> getLib lib mid "mid runScripts"
  --lift $ log $ inj "Running scripts [%0]" [joinWith ", " modD.scripts]
  foldM (runScript ssRef lib mid) kernels modD.scripts


runScript :: forall eff h. STRef h (SystemST h) -> Library h -> String -> (Set Kernel) -> String -> EpiS eff h (Set Kernel)
runScript ssRef lib mid kernels scr = do
  --lift $ log $ "Running Script : " <> scr <> " : for " <> mid
  --lift $ log x
  (Script name phase args) <- parseScript scr
  systemST <- lift $ readSTRef ssRef
  fn <- lookupScriptFN name
  let t' = systemST.t - phase

  mod@(Module _ modD) <- getLib lib mid "mid! runScript"
  idx <- fromJustE (elemIndex scr modD.scripts) "script not found m"

  (ScriptRes mut update) <- fn ssRef lib t' mid scr args -- run script
  case update of
    Just dt -> do
      let new = serializeScript (Script name phase dt)
      modD'     <- mD <$> getLib lib mid "mid' runScript"
      idx'      <- fromJustE (elemIndex scr modD'.scripts) "script not found m'"
      scripts'  <- fromJustE (updateAt idx' new modD'.scripts) "should be safe runScript"

      modLibD lib mod _ {scripts = scripts'}
    _ -> pure unit

  pure $ mut <> kernels
