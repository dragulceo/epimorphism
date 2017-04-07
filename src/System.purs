module System where

import Prelude
import Config (SystemST, defaultSystemST)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Data.Array (concat, foldM, sort, snoc)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Library (Library, dat, getLibM)
import Data.List (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Serialize (parseLibData)
import Data.Set (Set)
import Data.Set (member) as S
import Data.StrMap (values, empty, insert, fold, lookup, StrMap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Data.Types (Epi, EpiS, Module, Schema)
import Library (parseLib)
import SLibrary (SHandle, SLib, SLibError(..), buildComponent, buildIndex, parseSLib)
import Util (urlGet)

data DataSource = LocalHTTP | LocalStorage | RemoteDB

initSystemST :: forall eff h. String -> EpiS eff h (SystemST h)
initSystemST host = do
  -- gather system data here?  currently doing this in engine

  -- initialize libraries
  componentLib  <- buildSLib buildComponent  $ host <> "/lib/components.slib"
  indexLib      <- buildSLib buildIndex      $ host <> "/lib/indexes.slib"

  pure $ defaultSystemST {
      componentLib  = componentLib
    , indexLib      = indexLib
  }

initLibrary :: forall eff h. String -> EpiS eff h (Library h)
initLibrary host = do
  dt <- lift $ urlGet (host <> "/lib/new/core.lib")
  case dt of
    (Left er) -> throwError $ "Error loading library : " <> er
    (Right res) -> parseLibData res

buildLib :: forall eff a. Schema -> String -> Epi eff (StrMap a)
buildLib schema loc = do
  dt <- lift $ urlGet loc
  case dt of
    (Left er) -> throwError $ "Error loading lib : " <> er
    (Right res) -> parseLib schema res

-- build a shader library from a location with a builder
buildSLib :: forall eff a.  (SHandle -> SLib (Tuple String a)) -> String -> Epi eff (StrMap a)
buildSLib f loc = do
  dt <- lift $ urlGet loc
  case dt of
    (Left er) -> throwError $ "Error loading slib : " <> er
    (Right res) -> case (parseSLib f res) of
      (Right res') -> pure res'
      (Left (SLibError s)) -> throwError $ "Error building slib at : " <> loc <> " : " <> s


-- load from a map, throw error if not found. passed context for debugging purposes
loadLib :: forall eff a. String -> (StrMap a) -> String -> Epi eff a
loadLib name lib ctx = do
  case (lookup name lib) of
    (Just d) -> pure d
    Nothing  -> throwError ("Load from lib - can't find: " <> name <> ": context :" <> ctx)


-- FLAG FAMILYS & SO FORTH
checkFlags :: forall r. {flags :: Set String | r} -> Array String -> Array String -> Boolean
checkFlags obj inc exc = (foldl (\dt f -> dt && S.member f obj.flags) true inc) &&
                         (foldl (\dt f -> dt && (not $ S.member f obj.flags)) true exc)

-- filter a family by specific include & exclude flags, return the keys sorted alphabetically
flagFamily :: forall r. StrMap {flags :: Set String | r} -> Array String -> Array String -> Array String
flagFamily col inc exc = sort $ fold handle [] col
  where
    handle res k v = case (checkFlags v inc exc) of
      true -> snoc res k
      false -> res

-- filter a family by family, include & exclude flags, return the keys sorted alphabetically
family :: forall r. StrMap {family :: String, flags :: Set String | r} -> String -> Array String -> Array String -> Array String
family col fam inc exc = flagFamily (fold handle empty col) inc exc
  where
    handle res k v = case (v.family == fam) of
      true -> insert k v res
      false -> res


-- implement a breadth first fold over the modules rooted at mid
mFold :: forall eff h a. Library h -> a -> String -> (a -> String -> EpiS eff h a) -> EpiS eff h a
mFold lib val mid fn = bff val [mid]
  where
    bff :: a -> Array String -> EpiS eff h a
    bff tval [] = pure tval
    bff tval xs = do
      tval' <- (foldM fn tval xs)
      children <- traverse childValues xs
      bff tval' (concat children)
    childValues :: String -> EpiS eff h (Array String)
    childValues cid = do
      elt <- getLibM lib cid
      case (elt :: Maybe Module) of -- calling fn may have removed the module
        Just mod -> do
          pure $ toUnfoldable $ values (dat mod).modules
        Nothing -> pure []
