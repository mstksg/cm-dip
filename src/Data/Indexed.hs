{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}

module Data.Indexed where

import Data.Monoid
import Data.Maybe
import Control.Comonad

class RelIndex i r => Index i r where
    (?@) :: r a -> i -> Maybe a
    (?@) = (?)

indWith :: Index i r => a -> r a -> i -> a
indWith d x i = fromMaybe d (x ?@ i)

indWith0 :: (Num a, Index i r) => r a -> i -> a
indWith0 = indWith 0

mindWith :: (Monoid a, Index i r) => r a -> i -> a
mindWith = indWith mempty

(!@) :: Index i r => r a -> i -> a
x !@ i = fromJust (x ?@ i)

class (Index i r, TotalRelIndex i r) => TotalIndex i r where
    (#@) :: r a -> i -> a
    (#@) = (#)

class RelIndex i r where
    (?) :: r a -> i -> Maybe a

relWith :: RelIndex i r => a -> r a -> i -> a
relWith d x i = fromMaybe d (x ? i)

relWithW :: (Comonad r, RelIndex i r) => r a -> i -> a
relWithW x = relWith (extract x) x

relWith0 :: (Num a, RelIndex i r) => r a -> i -> a
relWith0 = relWith 0

mrelWith :: (Monoid a, RelIndex i r) => r a -> i -> a
mrelWith = relWith mempty

(!) :: RelIndex i r => r a -> i -> a
x ! i = fromJust (x ? i)

class RelIndex i r => TotalRelIndex i r where
    (#) :: r a -> i -> a
    (#) = (!)

