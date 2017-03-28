module EngineUtil where

import Prelude
import Config (Epi)
import Control.Monad.Except.Trans (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Either (either)
import Graphics.WebGL (runWebgl)
import Graphics.WebGL.Types (WebGL, WebGLContext)

-- execute a webgl action & wrap its error
execGL :: forall eff a. WebGLContext -> WebGL a -> Epi eff a
execGL ctx webGL = do
  res <- lift $ runWebgl webGL ctx
  either (throwError <<< show) pure res
