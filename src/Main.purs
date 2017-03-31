module Main where

import Prelude
import Compiler (compileShaders)
import Config (PMut(PMutNone, PMut), SystemST, UIST, EpiS, Pattern, EngineST, EngineConf, SystemConf, UIConf, CompOp(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (lift)
import Control.Monad.ST (ST, STRef, readSTRef, newSTRef, modifySTRef, runST)
import DOM (DOM)
import Data.Array (null, updateAt, foldM, sort, concatMap, fromFoldable)
import Data.Int (round, toNumber)
import Data.Maybe (isNothing, fromMaybe, Maybe(Nothing, Just))
import Data.Set (member)
import Data.StrMap (insert, values, keys, lookup)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Engine (postprocessFrame, initEngineST, renderFrame)
import Graphics.Canvas (CANVAS)
import Layout (updateLayout)
import Paths (runPath)
import Pattern (importPattern)
import Script (runScripts)
import System (initSystemST, loadLib)
import Texture (loadImages)
import UI (initUIST)
import Util (Now, dbg, fromJustE, handleError, imag, inj, isDev, isHalted, now, real, requestAnimationFrame, rndstr, seedRandom, urlArgs, zipI)

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
  pure conf


initState :: forall eff h. SystemST h -> EpiS (now :: Now | eff) h (State h)
initState systemST = do
  --  init config
  systemName <- lift $ getSysConfName
  systemConf <- loadLib systemName systemST.systemConfLib "init system"

  seed <- case systemConf.seed of
    "" -> do
      newSeed <- lift rndstr
      dbg $ "GENERATING SEED: " <> newSeed
      pure newSeed
    _ -> pure systemConf.seed

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

  -- load image libraries
  index' <- fromJustE (lookup pattern.defaultImageLib systemST.indexLib) "can't find default image lib!"
  index <- fromJustE (lookup pattern.imageLib systemST.indexLib) "can't find image lib!"
  lift $ loadImages index'.lib index.lib

  -- import pattern
  importPattern ssRef pRef
  systemST' <- lift $ readSTRef ssRef

  -- init engine & ui states
  esRef <- initEngineST systemConf' engineConf systemST' uiConf.canvasId Nothing
  usRef <- initUIST ucRef ecRef esRef pRef scRef ssRef

  pure {ucRef, usRef, ssRef, scRef, ecRef, esRef, pRef}


animate :: forall h. (State h) -> Eff (canvas :: CANVAS, dom :: DOM, now :: Now, st :: ST h) Unit
animate state = handleError do
  t0 <- lift $ now
  -- unpack state
  {ucRef, usRef, ssRef, scRef, ecRef, esRef, pRef} <- pure state

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

  let delta = 0.02 * pattern.tSpd -- a fixed increment of time looks better
  --let delta = (currentTimeMS - lastTimeMS) * pattern.tSpd / 1000.0
  let pauseF = if systemST.paused then 0.0 else 1.0
  let t' = systemST.t + pauseF * delta
  lift $ modifySTRef ssRef (\s -> s {t = t', lastTimeMS = Just currentTimeMS})

  -- fps
  when (systemST.frameNum `mod` uiConf.uiUpdateFreq == 0) do
    let lastFpsTimeMS = fromMaybe currentTimeMS systemST.lastFpsTimeMS
    let fps = round $ (toNumber uiConf.uiUpdateFreq) * 1000.0 / (currentTimeMS - lastFpsTimeMS)
    lift $ modifySTRef ssRef (\s -> s {lastFpsTimeMS = Just currentTimeMS, fps = Just fps})
    pure unit

  t1 <- lift $ now
  -- run scripts if not compiling
  when (null engineST.compQueue) do
    sRes <- runScripts ssRef pRef
    case sRes of
      PMutNone -> pure unit
      PMut pattern' new -> do
        let queue = (if (member "main" new) then [CompMainShader, CompMainProg] else []) <>
                    (if (member "disp" new) then [CompDispShader, CompDispProg] else []) <>
                    [CompFinish]

        lift $ modifySTRef esRef (\es -> es {compQueue = queue})
        pure unit

  systemST' <- lift $ readSTRef ssRef
  engineST' <- lift $ readSTRef esRef

  -- execute compile queue
  when (not $ null engineST'.compQueue) do
    --t' <- lift $ now
    compileShaders systemConf ssRef engineConf esRef pRef (isNothing engineST.mainProg)
    --t'' <- lift $ now
    --dbg $ inj "COMPILE :%0ms" [show (t'' - t')]
    --currentTimeMS2 <- lift $ now
    --lift $ modifySTRef ssRef (\s -> s {lastTimeMS = Just currentTimeMS2})
    pure unit

  t2 <- lift $ now

  engineST'' <- lift $ readSTRef esRef
  systemST'' <- lift $ readSTRef ssRef
  pattern'   <- lift $ readSTRef pRef

  -- render!
  t3 <- lift $ now
  (Tuple parM znM) <- getParZn systemST'' (Tuple [] []) pattern'.main
  tex <- renderFrame systemST'' engineConf engineST'' pattern' parM znM systemST''.frameNum

  (Tuple parD znD) <- getParZn systemST'' (Tuple [] []) pattern'.disp
  postprocessFrame systemST'' engineConf engineST'' tex parD znD
  t4 <- lift $ now

  -- update ui
  updateLayout uiConf uiST systemST'' pattern' false
  t5 <- lift $ now

  -- request next frame
  halted <- lift $ isHalted
  unless halted do
    lift $ modifySTRef ssRef (\s -> s {frameNum = s.frameNum + 1})
    lift $ requestAnimationFrame animate state
  t6 <- lift $ now

  --dbg $ inj "BREAKDOWN: init:%0ms scripts:%1ms recompile:%2ms render:%3ms ui:%4ms next:%5ms" [show (t1 - t0), show (t2 - t1), show (t3 - t2), show (t4 - t3), show (t5 - t4), show (t6 - t5)]

  pure unit

-- recursively flatten par & zn lists in compilation order
getParZn :: forall eff h. SystemST h -> (Tuple (Array Number) (Array Number)) -> String -> EpiS eff h (Tuple (Array Number) (Array Number))
getParZn systemST (Tuple par zn) mid = do
  mRef <- loadLib mid systemST.moduleRefPool "mid getParZn"
  mod  <- lift $ readSTRef mRef
  let t = systemST.t

  znV <- traverse (runZnPath mRef t) (zipI mod.zn)
  let znV' = concatMap (\x -> [real x, imag x]) znV
  let zn' = zn <> znV'

  parV <- traverse (runParPath mRef t) (sort $ keys mod.par)
  let par' = par <> parV

  foldM (getParZn systemST) (Tuple par' zn') (fromFoldable $ values mod.modules)
  where
    runZnPath mRef t (Tuple idx val) = do
      (Tuple res remove) <- runPath t val
      when remove do -- replace with constant
        m <- lift $ readSTRef mRef
        zn' <- fromJustE (updateAt idx (show res) m.zn) "should be safe getParZn"
        lift $ modifySTRef mRef (\m' -> m' {zn = zn'})
        pure unit
      pure res
    runParPath mRef t key = do
      m <- lift $ readSTRef mRef
      val <- fromJustE (lookup key m.par) "cant find val getParZn"
      (Tuple res remove) <- runPath t val
      let res' = real res
      when remove do -- replace with constant
        let par' = insert key (show res') m.par
        lift $ modifySTRef mRef (\m' -> m' {par = par'})
        pure unit
      pure res'

main :: Eff (canvas :: CANVAS, dom :: DOM, now :: Now) Unit
main = do
  runST do
    handleError do
      systemST <- initSystemST host
      state    <- initState systemST
      lift $ animate state
