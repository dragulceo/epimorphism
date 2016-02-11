module Main where

import Prelude (Unit, unit, return, bind, ($), show)
import Data.Either (Either(Right, Left))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Alert (Alert)
import Control.Monad.Except.Trans (runExceptT, lift, ExceptT ())
import Control.Monad.ST (ST, STRef, writeSTRef, readSTRef, newSTRef, runST)
import Graphics.Canvas (Canvas)

import Engine (loadEngineConf, initEngine, render, EngineState, EngineConf)
import UI (loadUIConf)
import Pattern (loadPattern, updatePattern, Pattern)
import JSUtil (unsafeLog)

main :: Eff (console :: CONSOLE, alert :: Alert, canvas :: Canvas) Unit
main = runST do
  res <- runExceptT doMain
  case res of
    Left er -> log $ show er
    Right _ -> return unit

doMain :: forall h. ExceptT String (Eff (console :: CONSOLE, alert :: Alert, canvas :: Canvas, st :: ST h)) Unit
doMain = do
    engineConf <- loadEngineConf "default"
    uiConf     <- loadUIConf     "default"
    pattern    <- loadPattern    "default"

    ecRef  <- lift $ newSTRef engineConf
    uicRef <- lift $ newSTRef uiConf
    pRef   <- lift $ newSTRef pattern

    esRef <- initEngine ecRef pRef

    -- register ui handlers
    animate ecRef esRef pRef

animate :: forall h. (STRef h EngineConf) -> (STRef h EngineState) -> (STRef h Pattern) -> ExceptT String (Eff (console :: CONSOLE, alert :: Alert, canvas :: Canvas, st :: ST h)) Unit
animate ecRef esRef pRef = do
  engineConf  <- lift $ readSTRef ecRef
  engineState <- lift $ readSTRef esRef
  pattern     <- lift $ readSTRef pRef

  let pattern' = updatePattern pattern
  lift $ writeSTRef pRef pattern'
  render engineConf engineState pattern'

  liftEff $ requestAnimationFrame $ runExceptT (animate ecRef esRef pRef)









  --
  --
  --
  --update progState

  --res <- runWebgl ( do
  --  progData <- initWebGL 1024
  --  animate progData 0
  --) ctx

  --handleGLError res
