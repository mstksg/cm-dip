{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.PArray where

import Data.Array.IArray
import Control.Monad
import Control.Comonad

instance Num a => Num (a, a) where
    (x0, y0) + (x1, y1) = (x0 + x1, y0 + y1)
    (x0, y0) - (x1, y1) = (x0 - x1, y0 - y1)
    (x0, y0) * (x1, y1) = (x0 * x1, y0 * y1)
    abs (x0, y0)        = (abs x0, abs y0)
    signum (x0, y0)     = (signum x0, signum y0)
    fromInteger         = join (,) . fromInteger

data PArray i e = PArray (Array i e) i deriving (Functor, Eq, Show)

instance Ix i => Comonad (PArray i) where
    extract (PArray a i) = a ! i
    extend f (PArray a i) = let es' = map (\j -> (j, f (PArray a j))) (indices a)
                            in  PArray (array (bounds a) es') i
    duplicate (PArray a i) = let es' = map (\j -> (j, PArray a j)) (indices a)
                             in  PArray (array (bounds a) es') i

(?~) :: (Ix i, Num i, Num e) => PArray i e -> i -> e
(PArray a i) ?~ j | inRange (bounds a) k = a ! k
                  | otherwise            = 0
  where
    k = i + j

diff :: (Ix i, Num i, Fractional e) => PArray i e -> e
diff p = ((p ?~ 1) - (p ?~ (-1))) / 2

laplac :: (Ix i, Num i, Fractional e) => PArray (i, i) e -> e
laplac p = (p ?~ (-1, 0)) + (p ?~ (1, 0)) + (p ?~ (0, -1)) + (p ?~ (0, 1)) + 4 * (p ?~ (0, 0))

line :: Int -> Double -> Double -> PArray Int Double
line i x0 dx = PArray (array (-i,i) es) 0
  where
    es = map (\j -> (j, fromIntegral j * dx + x0)) (range (-i, i))

gridPA :: Int -> (Double, Double) -> (Double, Double) -> PArray (Int, Int) (Double, Double)
gridPA i (x0, y0) (dx, dy) = PArray (array ((-i,-i), (i,i)) es) 0
  where
    es = map (\(j, k) -> ((j, k), (fromIntegral j * dx + x0, fromIntegral k * dy + y0))) (range ((-i,i), (i,i)))

