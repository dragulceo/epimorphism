module Dev where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.ST (runST)
import DOM (DOM)
--import Data.Library (SystemConf(..), SystemConfD(..), apI)
import Data.Set (empty) as S
import Data.StrMap (empty) as SM
import Graphics.Canvas (CANVAS)
import Util (Now, handleError)

--scD :: Lens' SystemConf SystemConfD
--scD = lens (\(SystemConf _ d) -> d) (\(SystemConf i _ ) d -> SystemConf i d)
--
--sCo :: SystemConf
--sCo = SystemConf {id: "me", parent: "", flags: empty, props: SM.empty} {engineConf: "ec", uiConf: "ui", pattern: "pat", seed: ""}


main :: forall eff. Eff (console :: CONSOLE, canvas :: CANVAS, dom :: DOM, now :: Now | eff) Unit
main = do
  runST do
    handleError do
      --let sc = (SystemConf {id: "hi", parent: "", flags: S.empty, props: SM.empty}
--            (SystemConfD {engineConf: "ec", uiConf: "uc", pattern: "p", seed: ""}))

      --let sc' = apI sc $ \x -> x {id = "blorp"}
      liftEff $ logShow "hi"
