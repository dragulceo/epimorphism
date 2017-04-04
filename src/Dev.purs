module Dev where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.ST (runST)
import DOM (DOM)
import Graphics.Canvas (CANVAS)
import Util (Now, handleError)

main :: forall eff. Eff (console :: CONSOLE, canvas :: CANVAS, dom :: DOM, now :: Now | eff) Unit
main = do
  runST do
    handleError do
      liftEff $ logShow "hi!"
--      let a = "asdfasdf{{11111\n22222\n}}\n\nasdfasdfasd\n\n\n\nasdfasdf{{\n\n333\n\n}}asdfasdfasd"
--      res <- test a
  --    liftEff $ logShow res
