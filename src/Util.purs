module Util where

import Prelude
import Data.Either (Either(..))
import Data.Complex
import Data.Tuple (Tuple(..))
import Data.Int (fromString) as I
import Data.Maybe (Maybe(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Error.Class (throwError)

import Config -- should we do this

foreign import data Now :: !

-- simple js functions
foreign import unsafeNull :: forall a. a
foreign import lg :: forall a b. a -> b
foreign import tLg :: forall a b. a -> b
foreign import unsafeEval :: forall eff. String -> Eff eff Unit
foreign import winLog :: forall a eff. a -> Eff eff Unit
foreign import requestAnimationFrame :: forall eff a. Eff eff Unit -> Eff eff Unit
foreign import now :: forall eff. Eff (now :: Now | eff) Number
foreign import replaceAll :: String -> String -> String -> String



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

-- should these be here?
numFromStringE :: forall eff. String -> Epi eff Number
numFromStringE s = case (numFromString s) of
  (Just n) -> return n
  _ -> throwError $ "Expected : " ++ s ++ " : to be a number"


intFromStringE :: forall eff. String -> Epi eff Int
intFromStringE s = case (I.fromString s) of
  (Just i) -> return i
  _ -> throwError $ "Expected : " ++ s ++ " : to be an int"
