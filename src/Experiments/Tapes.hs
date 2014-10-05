module Experiments.Tapes where

import Control.Arrow
import Control.Comonad
import Data.Stream
import Data.Tape

sinT :: Tape Double
sinT = fmap (sin . (/10)) . unfoldT pred succ $ 0

deriv :: Tape Double -> Double
deriv (Tape (l :~ _) _ (r :~ _)) = (r - l) / 2 * 10

sinT' :: Tape Double
sinT' = deriv <<= sinT

sinT'' :: Tape Double
sinT'' = deriv <<= deriv <<= sinT

type FiniteTape a = ([a], a, [a])

mask :: Num a => FiniteTape a -> Tape a -> a
mask (mls, mx, mrs) (Tape ls x rs) = sum (zipWith (*) (reverse mls) (toListS ls))
                                   + mx * x
                                   + sum (zipWith (*) mrs (toListS rs))

blur :: Fractional a => Tape a -> a
blur = mask ([0.025,0.075,0.2],0.4,[0.2,0.075,0.025])

diffr :: Fractional a => Tape a -> a
diffr = mask ([-0.5],0,[0.5])

type Tape2D a = Tape (Tape a)

grid :: (Double, Double) -> Double -> Tape2D (Double, Double)
grid (x0, y0) d = unfoldT (fmap (first (subtract d))) (fmap (first (+ d)))
                . unfoldT (second (subtract d)) (second (+ d))
                $ (x0, y0)

line :: Num a => a -> a -> Tape a
line x0 dx = unfoldT (subtract dx) (+ dx) x0
