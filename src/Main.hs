module Main where

-- import Data.PArray
import Experiments.Tapes
import Control.Applicative
import Data.Tape2D
import Data.Indexed
import Control.Arrow
import Control.Comonad
import Control.Monad
import Data.Stream
import Codec.Picture
import Data.Tape
-- import Experiments.Repa
-- import Linear


-- data Store s a = Store (s -> a) s

-- instance Functor (Store s) where
--     fmap g (Store f i) = Store (g . f) i

-- instance Comonad (Store s) where
--     extract (Store f i) = f i
--     extend g (Store f i) = Store (\j -> g (Store f j)) i

fromImage :: (Pixel a, Num a) => Image a -> Tape2D a
fromImage im = fmap f (grid (0, 0) 1)
  where
    w = imageWidth im
    h = imageHeight im
    f (x, y) | x < 0  = 0
             | x >= w = 0
             | y < 0  = 0
             | y >= h = 0
             | otherwise = pixelAt im x y

toImage :: Pixel a => Image a -> Tape2D a -> Image a
toImage im tp = generateImage f (imageWidth im) (imageHeight im)
  where
    f x y = tp # (x, y)
    -- f t 0 0 = extract . extract $ t
    -- f t x 0 = f (shiftR t) (x - 1) 0
    -- f t 0 y = f (fmap shiftR t) 0 (y - 1)
    -- f t x y = f (fmap shiftR . shiftR $ t) (x - 1) (y - 1)

main :: IO ()
main = do
    -- Right (ImageY8 im) <- readImage "media/cameraman.jpg"
    -- let imTape = (fmap . fmap) fromIntegral $ fromImage im :: Tape2D Double
    --     imBlur = fmap (extend diffr) imTape :: Tape2D Double
    --     imBackI = (fmap . fmap) round imBlur
    --     imBack = toImage im imBackI
    -- -- print $ (takeS 40 . fmap (takeS 40) . fmap rightsT . rightsT) imTape
    -- -- print $ (imTape ~!!~ 2) ~!!~ 2
    -- putStrLn "hey"
    -- saveBmpImage "media/cameraman2.bmp" (ImageY8 imBack)
    return ()
    -- f
    -- print $ map deriv . take 20 . iterate shiftR $ sinT
    -- print $ takeT 20 sinT
    -- print $ takeT 20 (deriv <<= deriv <<= deriv <<= deriv <<= sinT)
    -- print $ takeT 20 sinT''
    -- print $ takeT 20 (deriv <<= sinT)
    -- print $ takeT 20 (deriv' <<= sinT)
    -- print $ takeT 5 . fmap (takeT 5) $ grid (0,0) 0.1
    -- print . toListFArray $ line 50 0 0.1
    -- let sinA = fmap sin (line 100 0 0.01)
    -- print . toListFArray $ sinA
    -- print $ ((10*) . diff) <<= sinA
    -- print $ ((10*) . diff) <<= ((10*) . diff) <<= sinA
    -- print $ ((10*) . diffr) <<= sinA
    -- print $ ((10*) . diffr) <<= ((10*) . diffr) <<= sinA
    -- print . drop 100 . toListFArray $ sinA
    -- -- print . toListFArray $ ((10*) . diffr) <<= sinA
    -- -- print . toListFArray $ ((10*) . diffr) <<= ((10*) . diffr) <<= sinA
    -- let diffs = iterate (((100 *) . diffr) <<=) sinA
    -- -- print . toListFArray $ diffs !! 4
    -- print . drop 100 . toListFArray $ diffs !! 8
    -- -- print . toListFArray $ diffr <<= line 10 0 1
    -- let sinA = fmap sin (line 0 0.1)
    --     dotAmt = 1000
    --     sinAMag = dot dotAmt sinA sinA
    -- print . takeT 20 $ sinA
    -- let diffs = iterate (((10*) . diffr) <<=) sinA
    -- print . takeT 20 $ diffs !! 4
    -- print . takeT 20 $ diffs !! 16
    -- -- print $ dot 200 sinA (diffs !! 4) / sinAMag
    -- mapM_ (\t -> print (dot dotAmt t sinA / sinAMag)) . take 40 $ diffs
    -- let diffs' = drop 4 diffs
    -- zipWithM_ (\t1 t2 -> print $ (dot dotAmt t1 t2 / dot dotAmt t1 t1)) (take 100 diffs) diffs'

dot :: Fractional a => Int -> Tape a -> Tape a -> a
dot n t1 t2 = mask (replicate n 1, 1, replicate n 1) (liftA2 (*) t1 t2)

proj :: Fractional a => Int -> Tape a -> Tape a -> a
proj n t1 t2 = dot n t1 t2 / dot n t2 t2
