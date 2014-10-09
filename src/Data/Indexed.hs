{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}

module Data.Indexed where

import Data.Maybe

class RelIndex i r => Index i r where
    (?@) :: r a -> i -> Maybe a
    (?@) = (?)

indWith :: Index i r => r a -> i -> a -> a
indWith x i d = fromMaybe d (x ?@ i)

(!@) :: Index i r => r a -> i -> a
x !@ i = fromJust (x ?@ i)

class (Index i r, TotalRelIndex i r) => TotalIndex i r where
    (#@) :: r a -> i -> a
    (#@) = (#)

class RelIndex i r where
    (?) :: r a -> i -> Maybe a

relWith :: RelIndex i r => r a -> i -> a -> a
relWith x i d = fromMaybe d (x ? i)

(!) :: RelIndex i r => r a -> i -> a
x ! i = fromJust (x ? i)

class RelIndex i r => TotalRelIndex i r where
    (#) :: r a -> i -> a
    (#) = (!)

