{-# LANGUAGE FlexibleContexts #-}

module Experiments.Tapes where

import Control.Arrow
import Data.Indexed
import Control.Applicative
import Control.Comonad
import Data.Stream
import Data.Tape2D
import Data.Tape

sinT :: Tape Double
sinT = fmap (sin . (/10)) . iterateT pred succ $ 0

deriv :: Tape Double -> Double
deriv (Tape (l :~ _) _ (r :~ _)) = (r - l) / 2 * 10

sinT' :: Tape Double
sinT' = deriv <<= sinT

sinT'' :: Tape Double
sinT'' = deriv <<= deriv <<= sinT

type FiniteTape a = ([a], a, [a])
type FiniteTape2D a = FiniteTape (FiniteTape a)

mask :: Num a => FiniteTape a -> Tape a -> a
mask (mls, mx, mrs) (Tape ls x rs) = sum (zipWith (*) (reverse mls) (toListS ls))
                                   + mx * x
                                   + sum (zipWith (*) mrs (toListS rs))

-- mask2d :: Num a => FiniteTape2D a -> Tape2D a -> a
-- mask2d (mls, mx, mrs) (Tape ls x rs) = undefined

blur :: Fractional a => Tape a -> a
blur = mask ([0.025,0.075,0.2],0.4,[0.2,0.075,0.025])

diffr :: Fractional a => Tape a -> a
diffr = mask ([-0.5],0,[0.5])

laplace :: (Fractional a, RelIndex (Int, Int) t, Comonad t) => t a -> a
laplace t = relWithW t (c (-1)   0 )
          + relWithW t (c   1    0 )
          + relWithW t (c   0  (-1))
          + relWithW t (c   0    1 )
          - extract t * 4
  where
    c :: Int -> Int -> (Int, Int)
    c = (,)

sharpen :: (Fractional a, RelIndex (Int, Int) t, Comonad t) => a -> t a -> a
sharpen k = liftA2 (\f f'' -> f - k * f'') extract laplace

avg :: (Fractional a, RelIndex (Int, Int) t, Comonad t) => Int -> t a -> a
avg r t = sum [ relWithW t (i, j) | i <- [-r..r], j <- [-r..r]] / ((2*fromIntegral r+1)^(2 :: Int))

gauss :: (RealFloat a, RelIndex (Int, Int) t, Comonad t) => a -> t a -> a
gauss r t = sum (liftA2 f [-r'..r'] [-r'..r']) * c
  where
    f i j = relWithW t (i, j) * exp (ec * d i j)
    d x y = fromIntegral (x * x + y * y)
    σ     = r / 2
    ec    = -1 / (2 * σ * σ)
    c     = 1 / (2 * pi * σ * σ)
    r'    = ceiling r :: Int

-- laplace t = sum $ zipWith (curry (relWithW t)) [1 :: Int]
--                                                [2 :: Int]
  -- where
  --   p = relWith t (0, 0) 0

-- type Tape2D a = Tape (Tape a)

-- outer layer: x's
-- inner layer: y's
grid :: Num a => (a, a) -> a -> Tape2D (a, a)
grid p0 d = iterateT2D (first (subtract d)) (first (+d)) (second (subtract d)) (second (+d)) p0

line :: Num a => a -> a -> Tape a
line x0 dx = iterateT (subtract dx) (+ dx) x0

