{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Experiments.Repa where

import Data.Array.Repa.Repr.Unboxed
import Data.Array.Repa as R
import Data.Functor.Identity
import qualified Data.Indexed as I
import Control.Comonad
-- import qualified Data.Vector.Unboxed as V

data FArray r sh a = FArray !(Array r sh a)
                            !sh
                            !sh

instance Shape sh => Functor (FArray D sh) where
    fmap f (FArray xs i e) = FArray (R.map f xs) i e

instance Shape sh => Comonad (FArray D sh) where
    extract (FArray xs i _)  = xs ! i
    extend f (FArray xs i e) = FArray xs' i e
      where
        xs' = fromFunction e g
        g i' = f (FArray xs i' e)

withCoKleisliArray :: (Unbox a, Num a) => (FArray D DIM2 a -> b) -> (Array D DIM2 a -> Array D DIM2 b)
withCoKleisliArray ck g = fromFunction e f
  where
    e = extent g
    FArray g' _ _ = ck <<= (FArray g (Z :. 0 :. 0) e)
    f i = g' ! i

instance I.RelIndex DIM2 (FArray D DIM2) where
    (FArray xs (Z:.y:.x) _) ? (Z:.y':.x') = Just $ xs ! (Z :. (y+y') :. (x+x'))

-- instance Num a => TotalRelIndex a (Store a) where
--     st # i = peeks (+i) st

-- instance Num a => Index a (Store a) where
--     st ?@ i = Just $ peek i st

-- instance Num a => TotalIndex a (Store a) where
--     st #@ i = peek i st

-- withCoKleisliArray ck g = fromFunction (Z :. y :. x) f
--   where
--     Z :. y :. x = extent g
--     st = ck <<= fromArray g
--     f (Z :. j :. i) = peek (V2 i j) st

-- data FArray sh a = FArray (Array D sh a)
--                           (Array D sh sh)
--                           sh

-- instance Shape sh => Functor (FArray sh) where
--     fmap f (FArray xs is i) = FArray (R.map f xs) is i

-- instance Shape sh => Comonad (FArray sh) where
--     extract (FArray xs _ i)    = xs ! i
--     duplicate (FArray xs is i) = FArray xs' is i
--       where
--         xs' = R.map (FArray xs is) is
--     extend f (FArray xs is i)  = FArray xs' is i
--       where
--         xs' = R.map (f . FArray xs is) is

-- (?~) :: (Num a, Shape sh) => FArray sh a -> sh -> a
-- (FArray xs _ i) ?~ j | inShape (extent xs) k = xs ! k
--                      | otherwise             = 0
--     where
--       k = addDim i j

-- diffr :: Fractional a => FArray DIM1 a -> a
-- diffr p = (p ?~ (Z :. 1) - p ?~ (Z :. -1)) / 2

-- toListFArray :: forall a sh. (Unbox a, Shape sh) => FArray sh a -> [a]
-- toListFArray (FArray xs _ _) = (toList :: Array U sh a -> [a]) . runIdentity . computeP $ xs

-- line :: Int -> Double -> Double -> FArray DIM1 Double
-- line i x0 dx = FArray xs is (Z :. (i + 1))
--   where
--     ext = Z :. (i*2+1) 
--     xs = delay . fromListUnboxed ext . fmap (\j -> fromIntegral j * dx + x0) $ [-i..i]
--     is = fromFunction ext id
