module Data.Kernels where

import Prelude
import Control.Alt (class Alt, (<|>))
import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, sequence, traverseDefault)
import Partial.Unsafe (unsafePartial)

-- KERNELS
data Kernel = Seed0 | Seed1 | Main | Disp | Vert
derive instance kEw  :: Eq Kernel
derive instance kOrd :: Ord Kernel

instance showKernel :: Show Kernel where
  show Seed0 = "Seed0"
  show Seed1 = "Seed1"
  show Main = "Main"
  show Disp = "Disp"
  show Vert = "Vert"

readK :: String -> Maybe Kernel
readK "seed0" = Just Seed0
readK "Seed0" = Just Seed0
readK "seed1" = Just Seed1
readK "Seed1" = Just Seed1
readK "main" = Just Main
readK "Main" = Just Main
readK "Disp" = Just Disp
readK "disp" = Just Disp
readK "Vert" = Just Vert
readK "vert" = Just Vert
readK _ = Nothing

-- KMaps
data KMap a = KMap a a a a a
derive instance kmapFunc :: Functor KMap

instance kmapApply :: Apply KMap where
  apply (KMap fx fy fz fw fv) (KMap x y z w v) = KMap (fx x) (fy y) (fz z) (fw w) (fv v)

instance kmapApplicative :: Applicative KMap where
  pure a = KMap a a a a a

instance kmapFold :: Foldable KMap where
  foldMap mp (KMap x y z w v) = (mp x) <> (mp y) <> (mp z) <> (mp w) <> (mp v)
  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance kmapTrav :: Traversable KMap where
  sequence (KMap mx my mz mw mv) =
    let sl = sequence [mx, my, mz, mw, mv]
    in (unsafePartial kmp) <$> sl
    where
      kmp :: forall a. Partial => Array a -> KMap a
      kmp [x, y, z, w, v] = (KMap x y z w v)
  traverse f ta = traverseDefault f ta

--instance kmapAlt :: Alt a => Alt (KMap (a b)) where
--  alt (KMap x y z w) (KMap x' y' z' w') = (KMap (x <|> x') (y <|> y') (z <|> z') (w <|> w'))

--instance kmapAlt :: Alt a => Alt KMap where
altK :: forall a x. Alt a => KMap (a x) -> KMap (a x) -> KMap (a x)
altK (KMap x y z w v) (KMap x' y' z' w' v') = (KMap (x <|> x') (y <|> y') (z <|> z') (w <|> w') (v <|> v'))
infixr 0 altK as <||>

kGet :: forall a. KMap a -> Kernel -> a
kGet (KMap x _ _ _ _) Seed0 = x
kGet (KMap _ x _ _ _) Seed1 = x
kGet (KMap _ _ x _ _) Main = x
kGet (KMap _ _ _ x _) Disp = x
kGet (KMap _ _ _ _ x) Vert = x

kSet :: forall a. KMap a -> Kernel -> a -> KMap a
kSet (KMap x y z w v) Seed0 x' = (KMap x' y z w v)
kSet (KMap x y z w v) Seed1 y' = (KMap x y' z w v)
kSet (KMap x y z w v) Main  z' = (KMap x y z' w v)
kSet (KMap x y z w v) Disp  w' = (KMap x y z w' v)
kSet (KMap x y z w v) Vert  v' = (KMap x y z w v')

kAcs :: forall a r. KMap ({ seed0 :: a, seed1 :: a, main :: a, disp :: a, vert :: a | r } -> a)
kAcs = KMap (_.seed0) (_.seed1) (_.main) (_.disp) (_.vert)

kWrt :: forall a r. KMap a ->
        { seed0 :: a, seed1 :: a, main :: a, disp :: a, vert :: a | r } ->
        { seed0 :: a, seed1 :: a, main :: a, disp :: a, vert :: a | r }
kWrt (KMap x y z w v) obj = obj { seed0 = x, seed1 = y, main = z, disp = w, vert = v }

kNames :: KMap String
kNames = KMap "seed0" "seed1" "main" "disp" "vert"
