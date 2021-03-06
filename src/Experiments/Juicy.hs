{-# LANGUAGE FlexibleContexts #-}

module Experiments.Juicy where

import Codec.Picture
import Control.Comonad
import Data.Indexed
import Data.Tape2D
import Experiments.Tapes
import Linear.V2

fromImageOT :: (Pixel a, Num a) => Image a -> OffsetTape2D Int a
fromImageOT im = indexFromT2D 0 . fmap f $ grid (0, 0) 1
  where
    w = imageWidth im
    h = imageHeight im
    f (x, y) | x < 0  = 0
             | x >= w = 0
             | y < 0  = 0
             | y >= h = 0
             | otherwise = pixelAt im x y

toImage :: (Pixel a, TotalIndex (V2 Int) f) => Image a -> f a -> Image a
toImage im tp = generateImage f (imageWidth im) (imageHeight im)
  where
    f x y = tp # V2 x y

withCoKleisliOT :: (Pixel a, Num a) => (OffsetTape2D Int a -> a)  -> (Image a -> Image a)
withCoKleisliOT ck im = toImage im . extend ck . fromImageOT $ im

