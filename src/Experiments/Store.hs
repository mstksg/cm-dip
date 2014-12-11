{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS -fno-warn-orphans #-}

module Experiments.Store where

-- import Experiments.Juicy
import Codec.Picture
import Control.Applicative
import Control.Comonad
import Control.Comonad.Store
import Data.Indexed
import Linear.V2

fromImage :: (Pixel a, Num a) => Image a -> Store (V2 Int) a
fromImage im = store f 0
  where
    w = imageWidth im
    h = imageHeight im
    f (V2 x y) | x < 0     = 0
               | x >= w    = 0
               | y < 0     = 0
               | y >= h    = 0
               | otherwise = pixelAt im x y

-- toImage :: (Pixel a) => Image a -> Store (V2 Int) a -> Image a
-- toImage im st = generateImage f (imageWidth im) (imageHeight im)
--   where
--     f x y = st

-- fromImage :: (Pixel a, Num a) => Image a -> Store ()
-- fromImage im = indexFromT2D (0,0) . fmap f $ grid (0, 0) 1
--   where
--     w = imageWidth im
--     h = imageHeight im
--     f (x, y) | x < 0  = 0
--              | x >= w = 0
--              | y < 0  = 0
--              | y >= h = 0
--              | otherwise = pixelAt im x y

toImageV2 :: (Pixel a, TotalIndex (V2 Int) f) => Image a -> f a -> Image a
toImageV2 im tp = generateImage f (imageWidth im) (imageHeight im)
  where
    f x y = tp # V2 x y

instance Num a => RelIndex a (Store a) where
    st ? i = Just $ peeks (+i) st

instance Num a => TotalRelIndex a (Store a) where
    st # i = peeks (+i) st

instance Num a => Index a (Store a) where
    st ?@ i = Just $ peek i st

instance Num a => TotalIndex a (Store a) where
    st #@ i = peek i st

withCoKleisli :: (Pixel a, Num a) => Image a -> (Store (V2 Int) a -> a) -> Image a
withCoKleisli im ck = toImageV2 im . extend ck . fromImage $ im

gaussS :: (RealFloat a, RelIndex (V2 Int) t, Comonad t) => a -> t a -> a
gaussS r t = sum (liftA2 f [-r'..r'] [-r'..r']) * c
  where
    f i j = relWithW t (V2 i j) * exp (ec * d i j)
    d x y = fromIntegral (x * x + y * y)
    σ     = r / 2
    ec    = -1 / (2 * σ * σ)
    c     = 1 / (2 * pi * σ * σ)
    r'    = ceiling r :: Int

