module Data.Script where

import Control.Monad.ST (STRef)
import Data.Kernels (Kernel)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.StrMap (StrMap)
import Data.System (SystemST)
import Data.Types (EpiS, Library)

-- sys -> time -> mid -> self -> args -> res
type ScriptFn eff h = STRef h (SystemST h) -> Library h -> Number -> String -> String -> StrMap String -> EpiS eff h ScriptRes

data ScriptConfig = ScriptConfig String
data ScriptRes = ScriptRes (Set Kernel) (Maybe (StrMap String)) -- possible new root, possibly updated state
data Script = Script String Number (StrMap String)
