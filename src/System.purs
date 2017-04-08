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
import Data.StrMap (StrMap, empty, fold, insert, lookup, values)
import Data.StrMap (foldM) as SM
import Data.String (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Data.Types (Epi, EpiS, Module, Schema, ModuleD, moduleSchema)
import Library (parseLib)
import SLibrary (SHandle, SLib, SLibError(..), buildComponent, buildIndex, parseSLib)
import Serialize (unsafeSerialize)
import Util (dbg, urlGet)

data DataSource = LocalHTTP | LocalStorage | RemoteDB

--serializeModules :: forall eff h. EpiS eff h String
--serializeModules = do
--  modules <- buildLib moduleSchema "/lib/modules.lib"
--
--  serialized <- SM.foldM serialize empty (modules :: StrMap ModuleD)
--
--  let all = (toUnfoldable $ values serialized) :: Array String
--  pure $ joinWith "\n\n" all
--  where
--    serialize :: StrMap String -> String -> ModuleD -> EpiS eff h (StrMap String)
--    serialize res k modD = do
--      ser <- unsafeSerialize moduleSchema Nothing modD
--      let ser' = "###Module\nid " <> k <> ser
--      pure $ insert k ser' res

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
  mod <- lift $ urlGet (host <> "/lib/new/all_modules.lib")

  case (dt <> mod) of
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
