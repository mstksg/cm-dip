module Main where

-- import Data.PArray
import Experiments.Tapes
import Control.Applicative
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

main :: IO ()
main = do
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
    let sinA = fmap sin (line 0 0.1)
        dotAmt = 1000
        sinAMag = dot dotAmt sinA sinA
    print . takeT 20 $ sinA
    let diffs = iterate (((10*) . diffr) <<=) sinA
    print . takeT 20 $ diffs !! 4
    print . takeT 20 $ diffs !! 16
    -- print $ dot 200 sinA (diffs !! 4) / sinAMag
    mapM_ (\t -> print (dot dotAmt t sinA / sinAMag)) . take 40 $ diffs
    let diffs' = drop 4 diffs
    zipWithM_ (\t1 t2 -> print $ (dot dotAmt t1 t2 / dot dotAmt t1 t1)) (take 100 diffs) diffs'

dot :: Fractional a => Int -> Tape a -> Tape a -> a
dot n t1 t2 = mask (replicate n 1, 1, replicate n 1) (liftA2 (*) t1 t2)

proj :: Fractional a => Int -> Tape a -> Tape a -> a
proj n t1 t2 = dot n t1 t2 / dot n t2 t2
