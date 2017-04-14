module Paths where

import Prelude
import Control.Monad.Except.Trans (throwError)
import Data.Array (uncons)
import Data.Complex (outCartesian, Cartesian(Cartesian), Polar(Polar), outPolar, Complex)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)
import Data.Tuple (Tuple(Tuple))
import Data.Types (EpiS)
import Math (pi, min, cos, floor)
import Util (cxFromString, cxFromStringE, numFromStringE)


runPath :: forall eff h. Number -> String -> EpiS eff h (Tuple Complex Boolean)
runPath t pathStr = do
  Path func conf {spd, phase, args} <- parsePath pathStr
  func ((t - phase) * spd) args

------------------------------ PARSING ------------------------------

type PathFunc eff h = Number -> (Array String) -> EpiS eff h (Tuple Complex Boolean)

type PathArgs = {spd :: Number, phase :: Number, args :: Array String}
data PathConfig = PathConfig String
data Path eff h = Path (PathFunc eff h) PathConfig PathArgs

isConstantPath :: String -> Boolean
isConstantPath path = case (cxFromString path) of
  Just _ -> true
  Nothing -> false

parsePath :: forall eff h. String -> EpiS eff h (Path eff h)
parsePath dta = do
  let dta' = split (Pattern " ") $ trim dta

  {head: name, tail: allargs} <- case (cxFromString dta) of
    Just (Tuple r i) -> do
      pure {head: "const", tail: ["0.0", show $ outCartesian (Cartesian r i)]}
    _ -> do
      case uncons dta' of
        Just x -> pure x
        _ -> throwError "invalid path syntax"

  (Tuple name' phase) <- case (split (Pattern "@") name) of
    [x] -> pure $ Tuple x 0.0
    [x, y] -> do
      y' <- numFromStringE y
      pure $ Tuple x y'
    _ -> throwError "wtf are you doing?"

  {head: spd', tail: args} <- case uncons allargs of
    Just x -> pure x
    _ -> throwError "first arg must be spd"

  Tuple func conf <- getPathObj name'

  spd <- numFromStringE spd'
  pure $ Path func conf {spd, phase, args}


getPathObj :: forall eff h. String -> EpiS eff h (Tuple (PathFunc eff h) PathConfig)
getPathObj name = do
  case name of
    "const"  -> pure $ Tuple cnst (PathConfig "z")
    "linear" -> pure $ Tuple linear (PathConfig "t")
    "loop"   -> pure $ Tuple loop (PathConfig "mod t 1")
    "smooth" -> pure $ Tuple smooth (PathConfig "t * t * (3 - 2 * t)")
    "wave"   -> pure $ Tuple wave (PathConfig "a * cos(2.0 * pi * t) + b")
    "intrp"  -> pure $ Tuple intrp (PathConfig "(1-t) * z0 + t * z1")
    "linx"   -> pure $ Tuple linx (PathConfig "t + 0*i")
    "liny"   -> pure $ Tuple liny (PathConfig "t*i")
    "circle" -> pure $ Tuple circle (PathConfig "r * e^(2 * pi * i * t")
    "rose"   -> pure $ Tuple rose (PathConfig "(2.0 * pi * t) + (a * cos(b * t) + c) * i")
    -- ""   -> pure $ Tuple (PFD ) (PathConfig "")
    _ -> throwError $ "unknown path: " <> name



-- 1D FUNCTIONS
cnst :: forall eff h. PathFunc eff h
cnst t args = do
  z <- case args of
    [x] -> pure x
    _ -> throwError "invalid arguments for const"

  z' <- cxFromStringE z
  pure $ Tuple z' false

linear :: forall eff h. PathFunc eff h
linear t args = do
  let z = outCartesian (Cartesian t 0.0)
  pure $ Tuple z false

loop :: forall eff h. PathFunc eff h
loop t args = do
  let x = (t - floor(t))
  let z = outCartesian (Cartesian x 0.0)
  pure $ Tuple z false

smooth :: forall eff h. PathFunc eff h
smooth t args = do
  let x = t * t * (3.0 - 2.0 * t)
  let z = outCartesian (Cartesian x 0.0)
  pure $ Tuple z false

wave :: forall eff h. PathFunc eff h
wave t args = do
  x <- case args of
    [a', b'] -> do
      a <- numFromStringE a'
      b <- numFromStringE b'
      pure $ a * cos(2.0 * pi * t) + b
    _ -> throwError "invalid arguments for wave"
  let z = outCartesian (Cartesian x 0.0)
  pure $ Tuple z false


-- 2D FUNCTIONS
intrp :: forall eff h. PathFunc eff h
intrp t args = do
  z <- case args of
    [fromR', fromTh', toR', toTh'] -> do
      fromR <- numFromStringE fromR'
      fromTh <- numFromStringE fromTh'
      toR <- numFromStringE toR'
      toTh <- numFromStringE toTh'
      let t' = min t 1.0
      let r = toTh * t' + fromTh * (1.0 - t')
      let th = toR * t' + fromR * (1.0 - t')
      pure $ outPolar $ Polar r th
    _ -> throwError "invalid arguments for intrp"

  pure $ Tuple z (t >= 1.0)

linx :: forall eff h. PathFunc eff h
linx t args = do
  let z = outCartesian $ Cartesian t 0.0
  pure $ Tuple z false

liny :: forall eff h. PathFunc eff h
liny t args = do
  let z = outCartesian $ Cartesian 0.0 t
  pure $ Tuple z false

circle :: forall eff h. PathFunc eff h
circle t args = do
  z <- case args of
    [r'] -> do
      r <- numFromStringE r'
      pure $ outPolar $ Polar (2.0 * pi * t) r
    _ -> throwError "invalid arguments for circle"

  pure $ Tuple z false

rose :: forall eff h. PathFunc eff h
rose t args = do
  z <- case args of
    [a', b', c'] -> do
      a <- numFromStringE a'
      b <- numFromStringE b'
      c <- numFromStringE c'
      pure $ outPolar $ Polar (2.0 * pi * t) (a * cos(b * t) + c)
    _ -> throwError "invalid arguments for rose"

  pure $ Tuple z false
