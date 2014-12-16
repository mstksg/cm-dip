{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadComprehensions #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE QuasiQuotes #-}

module Experiments.Filters where

-- import Control.Applicative
-- import Language.Haskell.Codo
import Control.Comonad
import Data.Indexed
import Linear.V2
import qualified Data.Array.Repa as R

laplace :: (Fractional a, RelIndex (V2 Int) t, Comonad t) => t a -> a
laplace t = relWithW t (v2 (-1)   0 )
          + relWithW t (v2   1    0 )
          + relWithW t (v2   0  (-1))
          + relWithW t (v2   0    1 )
          - extract t * 4
-- laplace t = extract t * 4 + sum [ relWithW t (v2 i j) | i <- [-1..1]
--                                                       , j <- [-1..1]
--                                                       ]
  where
    v2 :: Int -> Int -> V2 Int
    v2 = V2

sharpen :: (Fractional a, RelIndex (V2 Int) t, Comonad t) => a -> t a -> a
sharpen k = [ f - k * f' | f  <- extract
                         , f' <- laplace
                         ]

avg :: (Fractional a, RelIndex (V2 Int) t, Comonad t) => Int -> t a -> a
avg r t = (/tot) . sum $ [ relWithW t (V2 i j) | i <- [-r..r]
                                               , j <- [-r..r]
                                               ]
  where
    tot = (2 * fromIntegral r + 1) ^ (2 :: Int)

gauss :: (RealFloat a, RelIndex (V2 Int) t, Comonad t) => a -> t a -> a
gauss r t = (*c) . sum $ [ f i j | i <- [-r'..r']
                                 , j <- [-r'..r']
                                 ]
  where
    f i j = relWithW t (V2 i j) * exp (ec * d i j)
    d x y = fromIntegral (x * x + y * y)
    σ     = r / 2
    ec    = -1 / (2 * σ * σ)
    c     = 1 / (2 * pi * σ * σ)
    r'    = ceiling r :: Int

gaussE :: (RealFloat a, RelIndex R.DIM2 t, Comonad t) => a -> t a -> a
gaussE r t = (*c) . sum $ [ f i j | i <- [-r'..r']
                                  , j <- [-r'..r']
                                  ]
  where
    f i j = relWithW t (R.Z R.:. i R.:. j) * exp (ec * d i j)
    d x y = fromIntegral (x * x + y * y)
    σ     = r / 2
    ec    = -1 / (2 * σ * σ)
    c     = 1 / (2 * pi * σ * σ)
    r'    = ceiling r :: Int

