{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Tape where

import Control.Applicative
import Data.Indexed
import Data.Stream
import Control.Comonad

data Tape a = Tape { leftsT  :: Stream a
                   , headT   :: a
                   , rightsT :: Stream a
                   }

shiftL :: Tape a -> Tape a
shiftL (Tape (l :~ ls) x rs) = Tape ls l (x :~ rs)

shiftR :: Tape a -> Tape a
shiftR (Tape ls x (r :~ rs)) = Tape (x :~ ls) r rs

takeT :: Int -> Tape a -> ([a], a, [a])
takeT n (Tape ls x rs) = (reverse (takeS n ls), x, takeS n rs)

iterateT :: (a -> a) -> (a -> a) -> a -> Tape a
iterateT fl fr x = Tape (iterateS fl (fl x)) x (iterateS fr (fr x))

-- warning: behaves differenty from iterateT!  "unfolds", but the first
-- result on the *rightward* direction is taken to be the "central" value.
unfoldT :: (b -> (a, b)) -> (b -> (a, b)) -> b -> Tape a
unfoldT fl fr x = Tape ls y rs
  where
    ls        = unfoldS fl x
    (y :~ rs) = unfoldS fr x

-- foldlT :: ((b, Int) -> a -> (b, Bool)) -> (b -> b -> c) -> b -> Tape a -> c
-- foldlT f g = g ()
--   where
--     go n z (x :~ xs) = case f (z, n) x of
--                          (y, False) -> y
--                          (y, True)  -> y `seq` go (n + 1) y xs

-- foldrT :: ((a, Int) -> b -> b) -> Stream a -> b
-- foldrT f = go 0
--   where
--     go n (x :~ xs) = f (x, n) (go (n + 1) xs)

instance (Ord a, Num a) => RelIndex a Tape where
    t ? n | n < 0     = ls ? (n - 1)
          | n > 0     = rs ? (n - 1)
          | otherwise = Just x
      where
        Tape ls x rs = t

instance (Ord a, Num a) => TotalRelIndex a Tape where
    t # n | n < 0     = ls # (n - 1)
          | n > 0     = rs # (n - 1)
          | otherwise = x
      where
        Tape ls x rs = t

instance (Ord a, Num a) => Index a Tape where

instance (Ord a, Num a) => TotalIndex a Tape where

instance Functor Tape where
    fmap f (Tape ls x rs) = Tape (fmap f ls) (f x) (fmap f rs)

instance Applicative Tape where
    pure = iterateT id id
    Tape fls fx frs <*> Tape ls x rs = Tape (fls <*> ls) (fx x) (frs <*> rs)

instance Comonad Tape where
    extract = headT
    duplicate xs = Tape (iterateS shiftL (shiftL xs))
                        xs
                        (iterateS shiftR (shiftR xs))
    extend f xs  = Tape (fmap f (iterateS shiftL (shiftL xs)))
                        (f xs)
                        (fmap f (iterateS shiftR (shiftR xs)))

-- tape with a stored reference to some "origin"
data OffsetTape o a = OffsetTape { otTape   :: Tape a
                                 , otOffset :: o
                                 }

indexFromT :: o -> Tape a -> OffsetTape o a
indexFromT = flip OffsetTape

instance Functor (OffsetTape o) where
    fmap f (OffsetTape t o) = OffsetTape (fmap f t) o

instance Enum o => Comonad (OffsetTape o) where
    extract = extract . otTape
    duplicate (OffsetTape t o) = OffsetTape t' o
      where
        t' = OffsetTape <$> iterateT shiftL shiftR t
                        <*> iterateT succ pred o
    extend f (OffsetTape t o)  = OffsetTape t' o
      where
        t' = fmap f . OffsetTape <$> iterateT shiftL shiftR t
                                 <*> iterateT succ pred o

instance (Ord a, Num a) => RelIndex a (OffsetTape o) where
    (OffsetTape t _) ? n = t ? n

instance (Ord a, Num a) => TotalRelIndex a (OffsetTape o) where
    (OffsetTape t _) # n = t # n

instance (Ord a, Num a) => Index a (OffsetTape a) where
    (OffsetTape t o) ?@ n = t ? (n + o)

instance (Ord a, Num a) => TotalIndex a (OffsetTape a) where
    (OffsetTape t o) #@ n = t # (n + o)

-- instance Num i => Applicative (OffsetTape i) where
--     pure = flip OffsetTape 0 . pure
--     OffsetTape ft fi <*> OffsetTape xt xi = OffsetTape


