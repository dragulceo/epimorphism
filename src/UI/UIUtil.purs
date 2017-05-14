module UI.UIUtil where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.DOM.Simple.Element (querySelector)
import Data.DOM.Simple.Unsafe.Element (HTMLElement)
import Data.DOM.Simple.Window (globalWindow, document)
import Data.Types (Epi)
import Util (fromJustE)

findElt :: forall eff. String -> Epi eff HTMLElement
findElt id = do
  doc <- lift $ document globalWindow
  elt <- lift $ querySelector ("#" <> id) doc
  fromJustE elt $ "couldn't find element: #" <> id
