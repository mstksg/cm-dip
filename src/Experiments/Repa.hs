{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
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

data FArray r sh a = FArray { fArrayArray  :: !(Array r sh a)
                            , fArrayFocus  :: !sh
                            , fArrayExtent :: !sh
                            }

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

instance (Shape a, Num a) => I.RelIndex a (FArray D a) where
    FArray xs i _ ? p = Just $ xs ! (i + p)

instance (Shape a, Num a) => I.TotalRelIndex a (FArray D a) where
    FArray xs i _ # p = xs ! (i + p)

instance (Shape a, Num a) => I.Index a (FArray D a) where
    FArray xs _ _ ?@ p = Just $ xs ! p

instance (Shape a, Num a) => I.TotalIndex a (FArray D a) where
    FArray xs _ _ #@ p = xs ! p

instance Num Z where
    _ + _ = Z
    _ * _ = Z
    _ - _ = Z
    abs _ = Z
    negate _ = Z
    signum _ = Z
    fromInteger _ = Z

instance (Num a, Num b) => Num (a :. b) where
    (a:.b) + (c:.d) = (a+c):.(b+d)
    (a:.b) * (c:.d) = (a*c):.(b*d)
    (a:.b) - (c:.d) = (a-c):.(b-d)
    negate (a:.b)   = negate a :. negate b
    abs (a:.b)      = abs a :. abs b
    signum (a:.b)   = signum a :. signum b
    fromInteger x   = fromInteger x :. fromInteger x

-- test :: DIM2 -> DIM2 -> DIM2
-- test = (+)

-- instance Index DIM2 (FArray D DIM2) where
--     (FArray xs)

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
