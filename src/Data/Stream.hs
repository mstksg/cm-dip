module Data.Stream where

import Control.Applicative
import Control.Comonad

infixr 5 :~
data Stream a = a :~ Stream a

unfoldS :: (a -> a) -> a -> Stream a
unfoldS f x = x :~ unfoldS f (f x)

tailS :: Stream a -> Stream a
tailS (_ :~ xs) = xs

headS :: Stream a -> a
headS (x :~ _) = x

takeS :: Int -> Stream a -> [a]
takeS n _ | n <= 0 = []
takeS n (x :~ xs) = x : takeS (n-1) xs

toListS :: Stream a -> [a]
toListS (x :~ xs) = x : toListS xs

instance Functor Stream where
    fmap f (x :~ xs) = f x :~ fmap f xs

instance Applicative Stream where
    pure = unfoldS id
    (f :~ fs) <*> (x :~ xs) = f x :~ (fs <*> xs)

instance Monad Stream where
    return = pure
    xs >>= f = join' (fmap f xs)
      where
        join' ((x :~ _) :~ xss) = x :~ join' (fmap tailS xss)

instance Comonad Stream where
    extract = headS
    extend f xs = f xs :~ extend f (tailS xs)

