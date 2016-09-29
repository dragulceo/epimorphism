module Util where

import Prelude
import Config (Epi)
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (runExceptT)
import DOM (DOM)
import Data.Array (length)
import Data.Complex (inCartesian, Cartesian(Cartesian), outCartesian, Complex)
import Data.Either (Either(..), either)
import Data.Foldable (foldr)
import Data.Int (fromString) as I
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap)
import Data.String (split, joinWith)
import Data.Tuple (fst, Tuple(..))
import Graphics.Canvas (Canvas)

foreign import data Now :: !

-- simple js functions
foreign import unsafeNull :: forall a. a
foreign import lg :: forall a b. a -> b
foreign import stick :: forall a b. a -> b
foreign import tLg :: forall a b. a -> b
foreign import unsafeEval :: forall eff. String -> Eff eff Unit
foreign import winLog :: forall a eff. a -> Eff eff Unit
foreign import requestAnimationFrame :: forall eff a. (a -> Eff eff Unit) -> a -> Eff eff Unit
foreign import now :: forall eff. Eff (now :: Now | eff) Number
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
foreign import unsafeCast :: forall a b. a -> b

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
boolFromStringE s = return $ boolFromString s


numFromStringE :: forall eff. String -> Epi eff Number
numFromStringE s = case (numFromString s) of
  (Just n) -> return n
  _ -> throwError $ "Expected : " ++ s ++ " : to be a number"


intFromStringE :: forall eff. String -> Epi eff Int
intFromStringE s = case (I.fromString s) of
  (Just i) -> return i
  _ -> throwError $ "Expected : " ++ s ++ " : to be an int"


cxFromStringE :: forall eff. String -> Epi eff Complex
cxFromStringE s = case (cxFromString s) of
  (Just (Tuple r i)) -> return $ outCartesian (Cartesian r i)
  _ -> throwError $ "Expected : " ++ s ++ " : to be an cx"


handleError :: forall eff. Epi eff Unit -> Eff (canvas :: Canvas, dom :: DOM | eff) Unit
handleError epi = do
  res <- runExceptT epi
  either err return res
  where
    err msg = do
      halt
      winLog msg

spc :: Int -> String
spc 0 = ""
spc m = " " ++ spc (m - 1)

indentLines :: Int -> String -> String
indentLines n s = joinWith "\n" $ map (\x -> (spc n) ++ x) $ split "\n" s


inj :: String -> Array String -> String
inj template dt = do
  fst $ foldr handle (Tuple template ((length dt) - 1)) dt
    where
      handle elt (Tuple res idx) = (Tuple (replaceAll ("%" ++ (show idx)) elt res) (idx - 1))


real :: Complex -> Number
real cx = case (inCartesian cx) of
  Cartesian x y -> x


imag :: Complex -> Number
imag cx = case (inCartesian cx) of
  Cartesian x y -> y
