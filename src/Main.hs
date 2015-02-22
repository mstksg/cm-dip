{-# LANGUAGE FlexibleContexts #-}

module Main where

-- import Data.Tape2D
-- import Experiments.Juicy
import Codec.Picture
import Control.Applicative
import Control.Comonad              as C
import Control.Comonad.Store
import Data.Array.Repa              as R
import Data.Array.Repa.Repr.Unboxed
import Data.Functor
import Data.Functor.Identity
import Data.Indexed
import Data.Tape
import Data.Word
import Experiments.Filters
import Experiments.Repa
import Experiments.Store            ()
import Experiments.Tapes
import Linear.V2
import qualified Experiments.Repa2  as R2

constrain :: Ord c => c -> c -> c -> c
constrain mi ma = max mi . min ma

-- withCoKleisli' :: (Unbox a, Pixel a, Integral a, Num b, RealFrac c) => (Store (V2 Int) b -> c) -> (Image a -> Image a)
-- withCoKleisli' f im = withCoKleisli (round . constrain 0 255 . f . fmap fromIntegral) im

withCoKleisli :: (Unbox a, Num a, Pixel a) => (FArray D DIM2 a -> a) -> (Image a -> Image a)
withCoKleisli ck i = R2.toImage . withCoKleisliArray ck . R2.fromImage $ i

withCoKleisli' :: (Unbox a, Pixel a, Integral a, Num b, RealFrac c) => (FArray D DIM2 b -> c) -> (Image a -> Image a)
withCoKleisli' f im = withCoKleisli (round . constrain 0 255 . f . fmap fromIntegral) im


-- withCoKleisli' :: (Pixel a, Integral a, Num b, RealFrac c) => (Store (V2 Int) b -> c) -> (Image a -> Image a)
-- withCoKleisli' f im = withCoKleisli (round . constrain 0 255 . f . fmap fromIntegral) im

-- withCoKleisli' :: (Pixel a, Integral a, Num b, RealFrac c) => Image a -> (OffsetTape2D Int b -> c) -> Image a
-- withCoKleisli' im f = withCoKleisli im (round . constrain 0 255 . f . fmap fromIntegral)

main :: IO ()
main = do
    Right (ImageY8 im) <- readImage "media/cameraman.jpg"
    -- let imRes = withCoKleisli' (sharpen 0.5) im
    -- let imRes = withCoKleisli' (sharpen 0.4) im
    -- let imRes = withCoKleisli' (gauss 2 =<= gauss 2) im
    let imRes = withCoKleisli' (gaussE 2 =<= gaussE 2) im
    -- let imRes = withCoKleisli' (gaussE 2) im
    saveBmpImage "media/cameraman6.bmp" (ImageY8 imRes)
    return ()

dot :: Fractional a => Int -> Tape a -> Tape a -> a
dot n t1 t2 = mask (replicate n 1, 1, replicate n 1) (liftA2 (*) t1 t2)

proj :: Fractional a => Int -> Tape a -> Tape a -> a
proj n t1 t2 = dot n t1 t2 / dot n t2 t2
