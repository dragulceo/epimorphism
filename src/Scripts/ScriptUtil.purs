module ScriptUtil where

import Prelude
import Config (Script, Pattern, Module, EpiS, SystemST)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.ST (readSTRef, STRef)
import Control.Monad.Trans (lift)
import Data.Array (length, (!!), foldM)
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (member, insert, union, StrMap)
import Data.String (split)
import Data.Tuple (Tuple(Tuple))
import Pattern (findModule, ImportObj(ImportScript), importScript)
import System (loadLib)

-- create a script dynamically & import it
createScript :: forall eff h. STRef h (SystemST h) -> String -> String -> String -> StrMap String -> EpiS eff h String
createScript ssRef mid parent fn dt = do
  systemST <- lift $ readSTRef ssRef
  scr      <- loadLib parent systemST.scriptLib "create script"

  let scr' = scr {fn = fn, dt = union dt scr.dt}
  importScript ssRef (ImportScript scr') mid


-- recursively parse a script from a string
data ScrPS = ScrFn | ScrDt

parseAndImportScript :: forall eff h. STRef h (SystemST h) -> Pattern -> String -> String -> EpiS eff h Script
parseAndImportScript ssRef pattern addr dt = do
  systemST <- lift $ readSTRef ssRef
  mid <- case (member addr systemST.moduleRefPool) of
    true -> return addr
    false -> findModule systemST.moduleRefPool pattern addr true

  def <- loadLib "default" systemST.scriptLib "building script in inc"
  Tuple scr _ <- foldM (parseScript' systemST.moduleRefPool pattern) (Tuple def ScrFn) (split " " dt)

  importScript ssRef (ImportScript scr) mid

  return scr

parseScript' :: forall eff h. StrMap (STRef h Module) -> Pattern -> (Tuple Script ScrPS) -> String -> EpiS eff h (Tuple Script ScrPS)
parseScript' mpool pattern (Tuple scr ps) dt = do
  case ps of
    ScrFn -> do
      return $ Tuple scr {fn = dt} ScrDt
    ScrDt -> do
      let tok = split ":" dt
      case (length tok) of
        2 -> do
          let dt' = insert (fromJust $ tok !! 0) (fromJust $ tok !! 1) scr.dt
          let scr' = scr {dt = dt'}
          return $ Tuple scr' ScrDt
        _ -> throwError $ "invalid script data assignment :" ++ dt
