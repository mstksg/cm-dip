-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Experiments.Repa where

import Data.Array.Repa.Repr.Unboxed
import Data.Array.Repa as R
import Data.Functor.Identity
import Control.Comonad
-- import qualified Data.Vector.Unboxed as V

data FArray sh a = FArray (Array D sh a)
                          (Array D sh sh)
                          sh

instance Shape sh => Functor (FArray sh) where
    fmap f (FArray xs is i) = FArray (R.map f xs) is i

instance Shape sh => Comonad (FArray sh) where
    extract (FArray xs _ i)    = xs ! i
    duplicate (FArray xs is i) = FArray xs' is i
      where
        xs' = R.map (FArray xs is) is
    extend f (FArray xs is i)  = FArray xs' is i
      where
        xs' = R.map (f . FArray xs is) is

(?~) :: (Num a, Shape sh) => FArray sh a -> sh -> a
(FArray xs _ i) ?~ j | inShape (extent xs) k = xs ! k
                     | otherwise             = 0
    where
      k = addDim i j

diffr :: Fractional a => FArray DIM1 a -> a
diffr p = (p ?~ (Z :. 1) - p ?~ (Z :. -1)) / 2

toListFArray :: forall a sh. (Unbox a, Shape sh) => FArray sh a -> [a]
toListFArray (FArray xs _ _) = (toList :: Array U sh a -> [a]) . runIdentity . computeP $ xs

line :: Int -> Double -> Double -> FArray DIM1 Double
line i x0 dx = FArray xs is (Z :. (i + 1))
  where
    ext = Z :. (i*2+1) 
    xs = delay . fromListUnboxed ext . fmap (\j -> fromIntegral j * dx + x0) $ [-i..i]
    is = fromFunction ext id
