module Main where

import Prelude

import Data.Either (Either(Right, Left))
-- import Data.Maybe
-- import Data.Tuple
-- import Data.Array
-- import Data.TypedArray as T
-- import Data.Int (toNumber)
-- import Data.Function

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Alert (Alert)
--import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (runExceptT, ExceptT ())
-- import Control.Monad.Eff.Exception
-- import Control.Monad.Eff.Class
-- import Control.Monad.Reader.Class (ask)
-- import Control.Monad.Eff.Class (liftEff)
--
-- import Graphics.WebGL
-- import Graphics.WebGL.Types
-- import Graphics.WebGL.Context
-- import Graphics.WebGL.Shader
-- import Graphics.WebGL.Methods
import Graphics.Canvas (Canvas)
-- import Graphics.WebGL.Raw as GL
-- import Graphics.WebGL.Raw.Enums as GLE
-- import Graphics.WebGL.Raw.Types as GLT

--import JSUtil
--import Engine
import Config

loadConfig :: forall eff. String -> ExceptT String (Eff eff) { engConf :: EngineConfig } --, uiConf :: UIConfig, sysConf :: SystemConfig }
loadConfig name = do
  let engConf = defEngConf
  return { engConf: engConf }


main :: Eff (console :: CONSOLE, alert :: Alert, canvas :: Canvas) Unit
main = do
  --{ engConf: engConf, uiConf: uiConf, sysConf: sysConf } <- loadConfig "default"
  res <- runExceptT do
    loadConfig "default"

  handleError res
  where
    handleError (Left er) = log $ show er
    handleError (Right _) = return unit


  --progState <- loadProgramState "default"
  --uiState   <- initUI uiConf
  --eState    <- initEngine engConf
  -- register ui handlers
  --update progState

  --Just canvas <- liftEff $ getCanvasElementById "glcanvas"
  --Just ctx <- liftEff $ getWebglContext canvas
  --res <- runWebgl ( do
  --  progData <- initWebGL 1024
  --  animate progData 0
  --) ctx

  --handleGLError res
