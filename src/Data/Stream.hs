{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Stream where

import Control.Applicative
import Control.Comonad.Store
import Control.Comonad
import Data.Indexed

infixr 5 :~
data Stream a = a :~ Stream a

iterateS :: (a -> a) -> a -> Stream a
iterateS f x = x :~ iterateS f (f x)

unfoldS :: (b -> (a, b)) -> b -> Stream a
unfoldS f = go
  where
    go x = let (y, x') = f x
           in  y :~ go x'

tailS :: Stream a -> Stream a
tailS (_ :~ xs) = xs

headS :: Stream a -> a
headS (x :~ _) = x

takeS :: Int -> Stream a -> [a]
takeS n _ | n <= 0 = []
takeS n (x :~ xs) = x : takeS (n-1) xs

toListS :: Stream a -> [a]
toListS (x :~ xs) = x : toListS xs

foldlS :: ((b, Int) -> a -> (b, Bool)) -> b -> Stream a -> b
foldlS f = go 0
  where
    go n z (x :~ xs) = case f (z, n) x of
                         (y, False) -> y
                         (y, True)  -> y `seq` go (n + 1) y xs

foldrS :: ((a, Int) -> b -> b) -> Stream a -> b
foldrS f = go 0
  where
    go n (x :~ xs) = f (x, n) (go (n + 1) xs)

instance (Ord a, Num a) => RelIndex a Stream where
    s ? n | n <= 0    = Just x
          | otherwise = xs ? (n - 1)
      where
        x :~ xs = s

instance (Ord a, Num a) => TotalRelIndex a Stream where
    s # n | n <= 0    = x
          | otherwise = xs # (n - 1)
      where
        x :~ xs = s

instance (Ord a, Num a) => Index a Stream where

instance (Ord a, Num a) => TotalIndex a Stream where

instance Functor Stream where
    fmap f (x :~ xs) = f x :~ fmap f xs

instance Applicative Stream where
    pure = iterateS id
    (f :~ fs) <*> (x :~ xs) = f x :~ (fs <*> xs)

instance Monad Stream where
    return = pure
    xs >>= f = join' (fmap f xs)
      where
        join' ((x :~ _) :~ xss) = x :~ join' (fmap tailS xss)

instance Comonad Stream where
    extract = headS
    extend f xs@(_ :~ xst) = f xs :~ extend f xst
    duplicate xs = xs :~ duplicate xs

-- stream with offset to some "origin"
data OffsetStream o a = OffsetStream { osStream :: Stream a
                                     , osOffset :: o
                                     }

indexFrom :: o -> Stream a -> OffsetStream o a
indexFrom = flip OffsetStream

instance Functor (OffsetStream o) where
    fmap f (OffsetStream s o) = OffsetStream (fmap f s) o

instance Enum o => Comonad (OffsetStream o) where
    extract = extract . osStream
    duplicate (OffsetStream s o) = OffsetStream s' o
      where
        s' = OffsetStream <$> iterateS tailS s
                          <*> iterateS pred o
    extend f (OffsetStream s o)  = OffsetStream s' o
      where
        s' = fmap f . OffsetStream <$> iterateS tailS s
                                   <*> iterateS pred o

instance (Ord o, Num o, Enum o) => ComonadStore o (OffsetStream o) where
    pos (OffsetStream _ o) = o
    peek o (OffsetStream s _) = s # o

instance (Ord a, Num a) => RelIndex a (OffsetStream o) where
    (OffsetStream s _) ? n = s ? n

instance (Ord a, Num a) => TotalRelIndex a (OffsetStream o) where
    (OffsetStream s _) # n = s # n

instance (Ord a, Num a) => Index a (OffsetStream a) where
    (OffsetStream s o) ?@ n = s ? (n + o)

instance (Ord a, Num a) => TotalIndex a (OffsetStream a) where
    (OffsetStream s o) #@ n = s # (n + o)
