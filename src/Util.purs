module Util where

import Prelude
import Data.Types (EpiS, Epi)
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import Data.Array (length, zip, (..))
import Data.Complex (inCartesian, Cartesian(Cartesian), outCartesian, Complex)
import Data.Either (Either(..), either)
import Data.Foldable (foldr)
import Data.Int (fromString, fromNumber) as I
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap)
import Data.String (Pattern(..), split, joinWith)
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (RegexFlags(..), noFlags)
import Data.Tuple (fst, Tuple(..))
import Graphics.Canvas (CANVAS)

foreign import data Now :: !

-- simple js functions
foreign import unsafeNull :: forall a. a
foreign import lg :: forall a b. a -> b
foreign import elg :: forall a b eff. a -> Eff eff b
foreign import stick :: forall a b. a -> b
foreign import tLg :: forall a b. a -> b
foreign import unsafeEval :: forall eff. String -> Eff eff Unit
foreign import winLog :: forall a eff. a -> Eff eff Unit
foreign import requestAnimationFrame :: forall eff a. (a -> Eff eff Unit) -> a -> Eff eff Unit
foreign import now :: forall eff. Eff (now :: Now | eff) Number
foreign import now2 :: forall eff. Eff eff Number
foreign import replaceAll :: String -> String -> String -> String

foreign import halt :: forall eff. Eff eff Unit
foreign import isHalted :: forall eff. Eff eff Boolean
foreign import urlArgs :: forall eff. Eff eff (StrMap String)
foreign import isDev :: forall eff. Eff eff Boolean

foreign import uuid   :: forall eff. Eff eff String
foreign import rndstr :: forall eff. Eff eff String
foreign import gmod :: Int -> Int -> Int
foreign import seedRandom :: forall eff. String -> Eff eff Unit
foreign import random :: forall eff. Eff eff Number
foreign import randInt :: forall eff. Int -> Eff eff Int
foreign import isNumber :: String -> Boolean

foreign import unsafeSetAttr :: forall a b. a -> String -> b -> a
foreign import unsafeGetAttr :: forall a b. a -> String -> b
foreign import hasAttr :: forall a. a -> String -> Boolean
foreign import unsafeCast :: forall a b. a -> b

foreign import getProfileCookie :: forall eff. Eff eff String

-- ghetto
foreign import clickPause :: forall eff. Eff eff Unit

urlGet :: forall eff. String -> Eff eff (Either String String)
urlGet = urlGetImpl Left Right

foreign import urlGetImpl :: forall eff. (forall a. a -> Either a a)
                          -> (forall a. a -> Either a a)
                          -> String
                          -> Eff eff (Either String String)

numFromString :: String -> Maybe Number
numFromString = numFromStringImpl Just Nothing

foreign import numFromStringImpl :: (forall a. a -> Maybe a)
                              -> (forall a. Maybe a)
                              -> String
                              -> Maybe Number


cxFromString :: String -> Maybe (Tuple Number Number)
cxFromString = cxFromStringImpl Tuple Just Nothing

foreign import cxFromStringImpl :: (Number -> Number -> (Tuple Number Number))
                                -> (forall a. a -> Maybe a)
                                -> (forall a. Maybe a)
                                -> String
                                -> Maybe (Tuple Number Number)

foreign import boolFromString :: String -> Boolean

boolFromStringE :: forall eff. String -> Epi eff Boolean
boolFromStringE s = pure $ boolFromString s


numFromStringE :: forall eff. String -> Epi eff Number
numFromStringE s = case (numFromString s) of
  (Just n) -> pure n
  _ -> throwError $ "Expected : " <> s <> " : to be a number"


intFromStringE :: forall eff. String -> Epi eff Int
intFromStringE s = case (I.fromString s) of
  (Just i) -> pure i
  _ -> throwError $ "Expected : " <> s <> " : to be an int"


cxFromStringE :: forall eff. String -> Epi eff Complex
cxFromStringE s = case (cxFromString s) of
  (Just (Tuple r i)) -> pure $ outCartesian (Cartesian r i)
  _ -> throwError $ "Expected : " <> s <> " : to be an cx"

forceInt :: Number -> Int
forceInt n = case (I.fromNumber n) of
  (Just i) -> i
  Nothing -> 0


handleError :: forall eff. Epi eff Unit -> Eff (canvas :: CANVAS, dom :: DOM | eff) Unit
handleError epi = do
  res <- runExceptT epi
  either err pure res
  where
    err msg = do
      halt
      winLog msg

tryRegex :: forall eff. String -> Epi eff Regex
tryRegex s = case regex s noFlags of
    Right x -> pure x
    Left _ -> throwError "Invalid Regex"

tryRegex' :: forall eff. String -> RegexFlags -> Epi eff Regex
tryRegex' s flags = case regex s flags of
    Right x -> pure x
    Left _ -> throwError "Invalid Regex"


fromJustE :: forall a eff. Maybe a -> String -> Epi eff a
fromJustE (Just x) _  = pure x
fromJustE Nothing msg = throwError msg

zipI :: forall a. Array a -> Array (Tuple Int a)
zipI x = zip (0..(length x - 1)) x

spc :: Int -> String
spc 0 = ""
spc m = " " <> spc (m - 1)

indentLines :: Int -> String -> String
indentLines n s = joinWith "\n" $ map (\x -> (spc n) <> x) $ split (Pattern "\n") s


inj :: String -> Array String -> String
inj template dt = do
  fst $ foldr handle (Tuple template ((length dt) - 1)) dt
    where
      handle elt (Tuple res idx) = (Tuple (replaceAll ("%" <> (show idx)) elt res) (idx - 1))


real :: Complex -> Number
real cx = case (inCartesian cx) of
  Cartesian x y -> x


imag :: Complex -> Number
imag cx = case (inCartesian cx) of
  Cartesian x y -> y


dbg :: forall eff h a b. a -> EpiS eff h b
dbg a = lift $ elg a

dbg2 :: forall eff a b. a -> Epi eff b
dbg2 a = lift $ elg a
