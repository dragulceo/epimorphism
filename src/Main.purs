module Main where

import Prelude
import Config (SystemST, UIST, EpiS, Pattern, EngineST, EngineConf, SystemConf, UIConf)
import Control.Monad (unless, when)
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (ST, STRef, readSTRef, newSTRef, modifySTRef, runST)
import DOM (DOM)
import Data.Array (foldM, sort, concatMap, elemIndex)
import Data.Int (round, toNumber)
import Data.List (fromList)
import Data.Maybe (maybe, fromMaybe, Maybe(Nothing, Just))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (values, keys, lookup)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Engine (postprocessFrame, preloadImages, initEngineST, renderFrame, setShaders)
import Graphics.Canvas (Canvas)
import Layout (updateLayout)
import Paths (runPath)
import Pattern (importPattern)
import Script (runScripts)
import System (initSystemST, loadLib)
import UI (initUIST)
import Util (imag, real, inj, rndstr, Now, handleError, lg, isHalted, requestAnimationFrame, now, seedRandom, urlArgs, isDev)

host :: String
host = ""

type State h = {
    ucRef :: STRef h UIConf
  , usRef :: STRef h UIST
  , scRef :: STRef h SystemConf
  , ssRef :: STRef h (SystemST h)
  , ecRef :: STRef h EngineConf
  , esRef :: STRef h EngineST
  , pRef  :: STRef h Pattern
}


getSysConfName :: forall eff. Eff eff String
getSysConfName = do
  args <- urlArgs
  dev <- isDev
  let def = if dev then "dev" else "prod"
  let conf = fromMaybe def (lookup "system" args)
  return conf


initState :: forall eff h. SystemST h -> EpiS (now :: Now | eff) h (State h)
initState systemST = do
  -- init config
  systemName <- lift $ getSysConfName
  systemConf <- loadLib systemName systemST.systemConfLib "init system"

  seed <- case systemConf.seed of
    "" -> do
      newSeed <- lift rndstr
      let xxx = lg $ "GENERATING SEED: " ++ newSeed
      return newSeed
    _ -> return systemConf.seed

  let systemConf' = systemConf {host = host, seed = seed}
  lift $ seedRandom systemConf'.seed

  engineConf <- loadLib systemConf'.initEngineConf systemST.engineConfLib "init engine"
  uiConf     <- loadLib systemConf'.initUIConf systemST.uiConfLib "init ui"
  pattern    <- loadLib systemConf'.initPattern systemST.patternLib "init pattern"

  -- build strefs
  ssRef <- lift $ newSTRef systemST
  scRef <- lift $ newSTRef systemConf'
  ecRef <- lift $ newSTRef engineConf
  ucRef <- lift $ newSTRef uiConf
  pRef  <- lift $ newSTRef pattern

  -- import pattern
  importPattern ssRef pRef
  pattern'  <- lift $ readSTRef pRef
  systemST' <- lift $ readSTRef ssRef

  -- init engine & ui states
  esRef <- initEngineST systemConf' engineConf systemST' pattern' uiConf.canvasId Nothing
  usRef <- initUIST ucRef ecRef esRef pRef scRef ssRef

  return {ucRef, usRef, ssRef, scRef, ecRef, esRef, pRef}


animate :: forall h. (State h) -> Eff (canvas :: Canvas, dom :: DOM, now :: Now, st :: ST h) Unit
animate state = handleError do
  --t0 <- lift $ now
  -- unpack state
  {ucRef, usRef, ssRef, scRef, ecRef, esRef, pRef} <- return state

  uiConf     <- lift $ readSTRef ucRef
  uiST       <- lift $ readSTRef usRef
  systemST   <- lift $ readSTRef ssRef
  systemConf <- lift $ readSTRef scRef
  engineConf <- lift $ readSTRef ecRef
  engineST   <- lift $ readSTRef esRef
  pattern    <- lift $ readSTRef pRef

  -- update time
  currentTimeMS <- lift $ now
  let lastTimeMS = fromMaybe currentTimeMS systemST.lastTimeMS
  let delta = (currentTimeMS - lastTimeMS) * pattern.tSpd / 1000.0
  --let delta' = 0.05
  let delta' = if systemST.paused then 0.0 else delta
  let t' = systemST.t + delta'
  lift $ modifySTRef ssRef (\s -> s {t = t', lastTimeMS = Just currentTimeMS})

  -- fps
  when (systemST.frameNum `mod` uiConf.uiUpdateFreq == 0) do
    let lastFpsTimeMS = fromMaybe currentTimeMS systemST.lastFpsTimeMS
    let fps = round $ (toNumber uiConf.uiUpdateFreq) * 1000.0 / (currentTimeMS - lastFpsTimeMS)
    lift $ modifySTRef ssRef (\s -> s {lastFpsTimeMS = Just currentTimeMS, fps = Just fps})
    return unit

  --t1 <- lift $ now
  --t2 <- lift $ now
  --let a = lg (t2 - t1)

  recompile <- runScripts ssRef pRef
  --t3 <- lift $ now

  systemST' <- lift $ readSTRef ssRef

  when recompile do
    setShaders systemConf engineConf esRef systemST' pattern
    currentTimeMS2 <- lift $ now
    lift $ modifySTRef ssRef (\s -> s {lastTimeMS = Just currentTimeMS2})
    return unit

  engineST' <- lift $ readSTRef esRef
  systemST'' <- lift $ readSTRef ssRef

  -- render!
  --t4 <- lift $ now
  (Tuple par zn) <- flattenParZn systemST'' (Tuple [] []) pattern.main
  tex <- renderFrame systemST'' engineConf engineST' pattern par zn systemST'.frameNum

  (Tuple par zn) <- flattenParZn systemST'' (Tuple [] []) pattern.disp
  postprocessFrame systemST'' engineConf engineST' tex par zn

  --t5 <- lift $ now

  -- update ui
  updateLayout uiConf uiST systemST'' pattern false
  --t6 <- lift $ now

  -- request next frame
  halted <- lift $ isHalted
  unless halted do
    lift $ modifySTRef ssRef (\s -> s {frameNum = s.frameNum + 1})
    lift $ requestAnimationFrame animate state
  --t7 <- lift $ now

  --let a = lg $ inj "BREAKDOWN: init:%0ms paths:%1ms scripts:%2ms junk:%3ms render:%4ms ui:%5ms next:%6ms" [show (t1 - t0), show (t2 - t1), show (t3 - t2), show (t4 - t3), show (t5 - t4), show (t6 - t5), show (t7 - t6)]

  return unit

-- recursively flatten par & zn lists in compilation order
flattenParZn :: forall eff h. SystemST h -> (Tuple (Array Number) (Array Number)) -> String -> EpiS eff h (Tuple (Array Number) (Array Number))
flattenParZn systemST (Tuple par zn) mid = do
  mRef <- loadLib mid systemST.moduleRefPool "mid flattenParZn"
  mod  <- lift $ readSTRef mRef

  let t = systemST.t

  znV <- traverse (\x -> runPath false mRef t (showPos mod.zn x) x) mod.zn
  let znV' = concatMap (\x -> [real x, imag x]) znV
  let zn' = zn ++ znV'

  parV <- traverse (get mRef mod.par) (sort $ keys mod.par)
  let parV' = map real parV
  let par' = par ++ parV'

  foldM (flattenParZn systemST) (Tuple par' zn') (fromList $ values mod.modules)
  where
    get mRef dt k = runPath true mRef systemST.t k (fromJust $ lookup k dt)
    showPos dt x = show $ fromJust $ elemIndex x dt



preloadAux :: forall h. (SystemST h) -> String ->
              Eff (canvas :: Canvas, dom :: DOM, now :: Now, st :: ST h) Unit ->
              Eff (canvas :: Canvas, dom :: DOM, now :: Now, st :: ST h) Unit
preloadAux systemST libName callback = do
  maybe (return unit)
    (\x -> preloadImages x.lib callback)
    (lookup libName systemST.indexLib)


-- clean this shit up yo
main :: Eff (canvas :: Canvas, dom :: DOM, now :: Now) Unit
main = do
  confn <- getSysConfName -- hack!!!!!

  runST do
    handleError do
      sys <- initSystemST host
      lift $ preloadAux sys (if confn =="ponies" then "ponies" else "all_images") do
        handleError do
          st <- initState sys
          lift $ animate st
