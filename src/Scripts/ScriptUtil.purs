module ScriptUtil where

import Prelude
import Config (EpiS, SystemST)
import Control.Monad.ST (readSTRef, STRef)
import Control.Monad.Trans (lift)
import Data.Either (Either(Left))
import Data.StrMap (union, StrMap)
import Pattern (importScript)
import System (loadLib)

-- create a script dynamically & import it
createScript :: forall eff h. STRef h (SystemST h) -> String -> String -> String -> StrMap String -> EpiS eff h String
createScript ssRef mid parent fn dt = do
  systemST <- lift $ readSTRef ssRef
  scr      <- loadLib parent systemST.scriptLib "create script"

  let scr' = scr {fn = fn, dt = union dt scr.dt}
  importScript ssRef (Left scr') mid
