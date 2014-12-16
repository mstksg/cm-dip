{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Experiments.Repa2 where

import Codec.Picture
import Control.Comonad              as C
import Data.Array.Repa.Eval
import Control.Comonad.Store
import Data.Array.Repa              as R
import Data.Array.Repa.Repr.Unboxed
import Data.Functor.Identity
import Data.Indexed hiding ((!))
import Data.Word
import Linear.V2
import Data.Function.Memoize
import Data.Memoizing ()

fromArray :: (Unbox a, Source r a, Num a) => Array r DIM2 a -> Store (V2 Int) a
fromArray im = store f 0
  where
    (Z :. h :. w) = extent im
    f (V2 x y) | x < 0     = 0
               | x >= w    = 0
               | y < 0     = 0
               | y >= h    = 0
               | otherwise = im ! (Z :. y :. x)

-- fromImage :: (Pixel a, Num a) => Image a -> Store (V2 Int) a
-- fromImage im = store f 0
--   where
--     w = imageWidth im
--     h = imageHeight im
--     f (V2 x y) | x < 0     = 0
--                | x >= w    = 0
--                | y < 0     = 0
--                | y >= h    = 0
--                | otherwise = pixelAt im x y

toImageRepa :: (Unbox a, TotalIndex (V2 Int) f, Source r a) => Array r DIM2 a -> f a -> Array D DIM2 a
toImageRepa g tp = fromFunction (Z :. y :. x) f
  where
    Z :. y :. x = extent g
    f (Z:. j :. i) = tp # V2 i j

-- withCoKleisli :: (Unbox a, Num a, TotalIndex (V2 Int) f, Comonad f) => (f a -> a) -> (Array U DIM2 a -> Array D DIM2 a)
-- withCoKleisli ck im = flip toImageRepa im . C.extend ck . fromImage $ im

withCoKleisliArray :: (Unbox a, Num a, Source r a) => (Store (V2 Int) a -> a) -> (Array r DIM2 a -> Array D DIM2 a)
withCoKleisliArray ck g = fromFunction (Z :. y :. x) f
  where
    Z :. y :. x = extent g
    st = ck <<= fromArray g
    f (Z :. j :. i) = peek (V2 i j) st

withCoKleisli :: (Unbox a, Num a, Pixel a) => (Store (V2 Int) a -> a) -> (Image a -> Image a)
withCoKleisli ck i = toImage . withCoKleisliArray ck . fromImage $ i

fromImage :: (Pixel a, Unbox a, Num a) => Image a -> Array D DIM2 a
fromImage im = fromFunction (Z :. h :. w) f
  where
    h = imageHeight im
    w = imageWidth im
    f (Z :. y :. x)  | x < 0     = 0
                     | x >= w    = 0
                     | y < 0     = 0
                     | y >= h    = 0
                     | otherwise = pixelAt im x y

toImage :: forall a. (Unbox a, Pixel a, Source D a, Target U a) => Array D DIM2 a -> Image a
toImage g = generateImage f w h
  where
    Z :. h :. w = extent g
    g' = runIdentity $ computeP g :: Array U DIM2 a
    f i j = g' ! (Z :. j :. i)

-- withCoKleisli :: (Unbox a, Num a) => (Store (V2 Int) a -> a) -> (Image a -> Image a)
-- withCoKleisli ck im = 

-- toImageV2 :: (Pixel a, TotalIndex (V2 Int) f) => Image a -> f a -> Image a
-- toImageV2 im tp = generateImage f (imageWidth im) (imageHeight im)
--   where
--     f x y = tp # V2 x y
