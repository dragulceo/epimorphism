module Pattern where

import Prelude
import Data.StrMap
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT ())

import Config

-- ?
type PatternConf = {

}


defaultModule :: Module
defaultModule = {
    name: ""
  , id: ""
  , family: ""
  , modules: SubModules empty
  , par: empty
  , zn: []
  , images: []
  , sub: empty
}


defaultPattern :: Pattern
defaultPattern = {
    modules: empty
  , scripts: []
  , t: 0.0
  , tPhase: 0.0
  , tSpd: 1.0
}

-- PUBLIC
loadPattern :: forall eff. String -> ExceptT String (Eff eff) Pattern
loadPattern name = do
  let pattern = defaultPattern
  return pattern


updatePattern :: Pattern -> Number -> Pattern
updatePattern pattern t' =
  pattern {t = t'}
