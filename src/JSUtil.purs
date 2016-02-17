module JSUtil where

import Prelude (Unit)
import Data.Either (Either(Right, Left))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

foreign import data Now :: !

foreign import unsafeURLGet :: forall eff. String -> Eff eff String
foreign import unsafeNull :: forall a. a
foreign import unsafeLog :: forall a eff. a -> Eff eff Unit
foreign import reallyUnsafeLog :: forall a b. a -> b
foreign import unsafeEval :: forall eff. String -> Eff eff Unit

foreign import requestAnimationFrame :: forall eff a. Eff ( | eff) Unit -> Eff ( | eff) Unit

foreign import now :: forall e. Eff (now :: Now | e) Number
