module Data.Script where

import Prelude
import Control.Monad.ST (STRef)
import Data.Maybe (Maybe)
import Data.Set (Set, union)
import Data.StrMap (StrMap)
import Data.System (SystemST)
import Data.Types (Library, PatternD, EpiS)

-- Script
data PMut = PMutNone | PMut PatternD (Set String)
instance mutSemi :: Semigroup PMut where
  append (PMut p0 s0) (PMut p1 s1) = PMut p0 (union s0 s1) -- sketchy if p0 != p1
  append PMutNone x = x
  append x PMutNone = x

-- sys -> time -> mid -> self -> args -> res
type ScriptFn eff h = STRef h (SystemST h) -> Library h -> Number -> String -> String -> StrMap String -> EpiS eff h ScriptRes

data ScriptConfig = ScriptConfig String
data ScriptRes = ScriptRes PMut (Maybe (StrMap String)) -- possible new root, possibly updated state
data Script = Script String Number (StrMap String)
