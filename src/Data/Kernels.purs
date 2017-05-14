module Data.Kernels where

import Prelude
import Control.Alt (class Alt, (<|>))
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequence, traverseDefault)
import Partial.Unsafe (unsafePartial)

-- KERNELS
data Kernel = Seed | Main | Disp | Vert
derive instance kEw  :: Eq Kernel
derive instance kOrd :: Ord Kernel

instance showKernel :: Show Kernel where
  show Seed = "Seed"
  show Main = "Main"
  show Disp = "Disp"
  show Vert = "Vert"

readK :: String -> Maybe Kernel
readK "seed" = Just Seed
readK "Seed" = Just Seed
readK "main" = Just Main
readK "Main" = Just Main
readK "Disp" = Just Disp
readK "disp" = Just Disp
readK "Vert" = Just Vert
readK "vert" = Just Vert
readK _ = Nothing

-- KMaps
data KMap a = KMap a a a a
derive instance kmapFunc :: Functor KMap

instance kmapApply :: Apply KMap where
  apply (KMap fx fy fz fw) (KMap x y z w) = KMap (fx x) (fy y) (fz z) (fw w)

instance kmapApplicative :: Applicative KMap where
  pure a = KMap a a a a

instance kmapFold :: Foldable KMap where
  foldMap mp (KMap x y z w) = (mp x) <> (mp y) <> (mp z) <> (mp w)
  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance kmapTrav :: Traversable KMap where
  sequence (KMap mx my mz mw) =
    let sl = sequence [mx, my, mz, mw]
    in (unsafePartial kmp) <$> sl
    where
      kmp :: forall a. Partial => Array a -> KMap a
      kmp [x, y, z, w] = (KMap x y z w)
  traverse f ta = traverseDefault f ta

--instance kmapAlt :: Alt a => Alt (KMap (a b)) where
--  alt (KMap x y z w) (KMap x' y' z' w') = (KMap (x <|> x') (y <|> y') (z <|> z') (w <|> w'))

--instance kmapAlt :: Alt a => Alt KMap where
altK :: forall a x. Alt a => KMap (a x) -> KMap (a x) -> KMap (a x)
altK (KMap x y z w) (KMap x' y' z' w') = (KMap (x <|> x') (y <|> y') (z <|> z') (w <|> w'))
infixr 0 altK as <||>

kGet :: forall a. KMap a -> Kernel -> a
kGet (KMap x _ _ _) Seed = x
kGet (KMap _ x _ _) Main = x
kGet (KMap _ _ x _) Disp = x
kGet (KMap _ _ _ x) Vert = x

kSet :: forall a. KMap a -> Kernel -> a -> KMap a
kSet (KMap x y z w) Seed x' = (KMap x' y z w)
kSet (KMap x y z w) Main y' = (KMap x y' z w)
kSet (KMap x y z w) Disp z' = (KMap x y z' w)
kSet (KMap x y z w) Vert w' = (KMap x y z w')

kAcs :: forall a r. KMap ({ seed :: a, main :: a, disp :: a, vert :: a | r } -> a)
kAcs = KMap (_.seed) (_.main) (_.disp) (_.vert)

kWrt :: forall a r. KMap a ->
        { seed :: a, main :: a, disp :: a, vert :: a | r } ->
        { seed :: a, main :: a, disp :: a, vert :: a | r }
kWrt (KMap x y z w) obj = obj { seed = x, main = y, disp = z, vert = w }

kNames :: KMap String
kNames = KMap "seed" "main" "disp" "vert"
