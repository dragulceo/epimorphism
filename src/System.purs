module System where

import Prelude
import Data.Foreign
import Data.Foreign.Class
import Data.Array (uncons)
import Data.Either
import Data.String (split, joinWith)
import Data.Tuple
import Data.Maybe (Maybe(Nothing, Just))
import Data.StrMap (StrMap (), empty, fromFoldable, lookup)
import Data.Traversable
import Control.Monad.Eff (Eff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT (), lift)
import Control.Monad.ST (ST)

import Config
import JSUtil (unsafeURLGet, unsafeLog)

data DataSource = LocalHTTP | LocalStorage | RemoteDB

defaultSystemConf :: SystemConf
defaultSystemConf = {
    name: "default"
  , initEngineConf: "default"
  , initUIConf: "default"
  , initPattern: "default"
}

defaultSystemST :: SystemST
defaultSystemST = {
    lastTimeMS: Nothing
  , frameNum: 0
  , lastFpsTimeMS: Nothing
  , fps: Nothing
  , uiConfLib: []
  , engineConfLib: []
  , patternLib: []
  , moduleLib: []
  , shaderLib: empty
  , componentLib: empty
  , libraryLib: empty
}


initSystemST :: forall eff h. SystemConf -> Epi (st :: ST h | eff) SystemST
initSystemST systemConf = do
  let st = defaultSystemST
  -- load libraries

  -- engine
  ecl <- readJSON <$> (lift $ unsafeURLGet "/lib/engine_conf.json")
  engineConfLib <- handle unpackEngineConf ecl
  let st' = st {engineConfLib = engineConfLib}

  return st'

  where
    handle :: forall a b h eff. (a -> b) -> F (Array a) -> Epi (st :: ST h | eff) (Array b)
    handle f (Left err) = throwError $ show err
    handle f (Right res) = return $ map f res


--parseEngineConf :: forall eff. StrMap String -> ExceptT String (Eff eff) EngineConf
--parseEngineConf dta = do
--  return {name: "", kernelDim: 100, fract: 3}
--  name      <- handle $ Data.StrMap.lookup "name" dta
--  kernelDim <- handle $ Data.StrMap.lookup "kernelDim" dta
--  fract     <- handle $ Data.StrMap.lookup "fract" dta
--
--  return {name: name, kernelDim: kernelDim, fract: fract}
--  where
--    handle (Just elt) = return elt
--    handle Nothing = throwError "shiz"
--

--parse :: forall eff. String -> ExceptT String (Eff eff) (Array (StrMap String))
--parse url = do
--  txt <- lift $ unsafeURLGet url
--  traverse parseGroup (split "\n\n" txt)
--
--
--parseGroup :: forall eff. String -> ExceptT String (Eff eff) (StrMap String)
--parseGroup txt = do
--  fromFoldable <$> traverse parseBit (split "\n" txt)
--
--  where
--    parseBit bit = case (uncons $ split " " bit) of
--      Just {head: name, tail: dta} -> return $ (Tuple name (joinWith " " dta))
--      Nothing -> throwError $ "error parsing bit: " ++ bit
