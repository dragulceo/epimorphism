module System where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (lift)
import Data.Array (concat, foldM)
import Data.Either (Either(..))
import Data.Library (dat, getLibM)
import Data.List (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Serialize (parseLibData)
import Data.StrMap (StrMap, lookup, values)
import Data.Traversable (traverse)
import Data.Types (Epi, EpiS, Module, Library)
import Util (dbg, inj, urlGet)

initLibrary :: forall eff h. String -> EpiS eff h (Library h)
initLibrary host = do
  core <- lift $ urlGet (host <> "/lib/core.lib")
  user <- lift $ urlGet (host <> "/lib/user.lib")

  let sep = Right "\n@@@ Sections\n"  -- dont hard code this
  sections <- lift $ urlGet (host <> "/lib/sections.slib")
  case (core <> user <> sep <> sections) of
    (Left er) -> throwError $ "Error loading library : " <> er
    (Right res) -> parseLibData res

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
