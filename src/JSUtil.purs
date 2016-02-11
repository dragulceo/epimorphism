module JSUtil where

import Prelude (Unit)
import Control.Monad.Eff (Eff)

foreign import unsafeURLGet :: forall eff. String -> Eff eff String
foreign import unsafeNull :: forall a. a
foreign import unsafeLog :: forall a eff. a -> Eff eff Unit

foreign import requestAnimationFrame :: forall eff a. Eff (console :: CONSOLE | eff) (Either WebGLError a) -> Eff ( | eff) Unit
