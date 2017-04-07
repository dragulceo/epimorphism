module UIUtil where

import Prelude
import Control.Monad.Except.Trans (throwError)
import Control.Monad.Trans.Class (lift)
import Data.DOM.Simple.Element (querySelector)
import Data.DOM.Simple.Unsafe.Element (HTMLElement)
import Data.DOM.Simple.Window (globalWindow, document)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Types (Epi)

findElt :: forall eff. String -> Epi eff HTMLElement
findElt id = do
  doc <- lift $ document globalWindow
  elt <- lift $ querySelector ("#" <> id) doc

  case elt of
    (Just e) -> pure e
    Nothing  -> throwError $ "couldn't find element: #" <> id
