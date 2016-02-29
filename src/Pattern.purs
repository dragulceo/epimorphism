module Pattern where

import Prelude
import Data.StrMap
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT ())

import Config
