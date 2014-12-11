{-# LANGUAGE FlexibleContexts #-}

module Main where

-- import Data.Tape2D
-- import Experiments.Juicy
import Codec.Picture
import Control.Applicative
import Control.Comonad.Store
import Data.Tape
import Experiments.Store
import Experiments.Tapes
import Linear.V2

constrain :: Ord c => c -> c -> c -> c
constrain mi ma = max mi . min ma

withCoKleisli' :: (Pixel a, Integral a, Num b, RealFrac c) => Image a -> (Store (V2 Int) b -> c) -> Image a
withCoKleisli' im f = withCoKleisli im (round . constrain 0 255 . f . fmap fromIntegral)

-- withCoKleisli' :: (Pixel a, Integral a, Num b, RealFrac c) => Image a -> (OffsetTape2D Int b -> c) -> Image a
-- withCoKleisli' im f = withCoKleisli im (round . constrain 0 255 . f . fmap fromIntegral)

main :: IO ()
main = do
    Right (ImageY8 im) <- readImage "media/cameraman.jpg"
    -- let imRes = withCoKleisli' im (sharpen 0.4)
    let imRes = withCoKleisli' im (gaussS 5)
    saveBmpImage "media/cameraman4.bmp" (ImageY8 imRes)
    return ()

dot :: Fractional a => Int -> Tape a -> Tape a -> a
dot n t1 t2 = mask (replicate n 1, 1, replicate n 1) (liftA2 (*) t1 t2)

proj :: Fractional a => Int -> Tape a -> Tape a -> a
proj n t1 t2 = dot n t1 t2 / dot n t2 t2
