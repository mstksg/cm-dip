module Data.Tape where

import Control.Applicative
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

unfoldT :: (a -> a) -> (a -> a) -> a -> Tape a
unfoldT fl fr x = Tape (unfoldS fl (fl x)) x (unfoldS fr (fr x))

instance Functor Tape where
    fmap f (Tape ls x rs) = Tape (fmap f ls) (f x) (fmap f rs)

instance Applicative Tape where
    pure = unfoldT id id
    Tape fls fx frs <*> Tape ls x rs = Tape (fls <*> ls) (fx x) (frs <*> rs)

instance Comonad Tape where
    extract = headT
    extend f xs = Tape (fmap f (unfoldS shiftL (shiftL xs)))
                       (f xs)
                       (fmap f (unfoldS shiftR (shiftR xs)))
