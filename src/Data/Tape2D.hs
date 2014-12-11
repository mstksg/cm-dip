{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Tape2D where

import Data.Tape
import Control.Arrow
import Control.Lens.Setter
import Control.Comonad
import Control.Applicative
import Data.Indexed
import Linear.V2

newtype Tape2D a = Tape2D { getTapes :: Tape (Tape a)
                          }

iterateT2D :: (a -> a)  -- ^ left
           -> (a -> a)  -- ^ right
           -> (a -> a)  -- ^ down
           -> (a -> a)  -- ^ up
           -> a         -- ^ initial
           -> Tape2D a
iterateT2D l r d u = Tape2D . fmap (iterateT d u) . iterateT l r

-- like in unfoldT, the "central" line is the first result of the rightward
-- unfolder. different than iterateT2D
--
-- actually, fix this...this is all kinds of messed up
unfoldT2D :: (b -> b)       -- ^ left
          -> (b -> b)       -- ^ right
          -> (b -> (a, b))  -- ^ up
          -> (b -> (a, b))  -- ^ down
          -> b              -- ^ initial
          -> Tape2D a
unfoldT2D l r d u = Tape2D . unfoldT (unfolder l) (unfolder r)
  where
    unfolder f x = let x' = f x
                   in  (unfoldT d u x', x')

shiftL2D :: Tape2D a -> Tape2D a
shiftL2D (Tape2D t) = Tape2D (shiftL t)

shiftR2D :: Tape2D a -> Tape2D a
shiftR2D (Tape2D t) = Tape2D (shiftR t)

shiftD2D :: Tape2D a -> Tape2D a
shiftD2D (Tape2D t) = Tape2D (fmap shiftL t)

shiftU2D :: Tape2D a -> Tape2D a
shiftU2D (Tape2D t) = Tape2D (fmap shiftR t)

instance (Ord a, Num a) => RelIndex (V2 a) Tape2D where
    Tape2D t ? (V2 x y) = ys ? y
      where
        ys = t # x

instance (Ord a, Num a) => TotalRelIndex (V2 a) Tape2D where
    Tape2D t # (V2 x y) = ys # y
      where
        ys = t # x

instance (Ord a, Num a) => Index (V2 a) Tape2D
instance (Ord a, Num a) => TotalIndex (V2 a) Tape2D

instance Functor Tape2D where
    fmap f (Tape2D t) = Tape2D . (fmap . fmap) f $ t

instance Applicative Tape2D where
    pure = iterateT2D id id id id
    Tape2D tf <*> Tape2D tx = Tape2D $ liftA2 (<*>) tf tx

instance Comonad Tape2D where
    extract (Tape2D t)   = extract . extract $ t
    duplicate = iterateT2D shiftL2D shiftR2D shiftD2D shiftU2D

indexFromT2D :: V2 o -> Tape2D a -> OffsetTape2D o a
indexFromT2D = flip OffsetTape2D

-- tape2d with a stored reference to some "origin"
data OffsetTape2D o a = OffsetTape2D { ot2dTape   :: Tape2D a
                                     , ot2dOffset :: V2 o
                                     }

instance Functor (OffsetTape2D o) where
    fmap f (OffsetTape2D t o) = OffsetTape2D (fmap f t) o

instance Enum o => Comonad (OffsetTape2D o) where
    extract = extract . ot2dTape
    duplicate (OffsetTape2D t o) = OffsetTape2D t' o
      where
        t' = OffsetTape2D <$> iterateT2D shiftL2D shiftR2D shiftD2D shiftU2D t
                          <*> iterateT2D (over _x succ) (over _x pred) (over _y succ) (over _y pred) o
    extend f (OffsetTape2D t o)  = OffsetTape2D t' o
      where
        t' = fmap f . OffsetTape2D <$> iterateT2D shiftL2D shiftR2D shiftD2D shiftU2D t
                                   <*> iterateT2D (over _x succ) (over _x pred) (over _y succ) (over _y pred) o

instance (Ord a, Num a) => RelIndex (V2 a) (OffsetTape2D a) where
    OffsetTape2D (Tape2D t) _ ? (V2 x y) = ys ? y
      where
        ys = t # x

instance (Ord a, Num a) => TotalRelIndex (V2 a) (OffsetTape2D a) where
    OffsetTape2D (Tape2D t) _ # (V2 x y) = ys # y
      where
        ys = t # x

instance (Ord a, Num a) => Index (V2 a) (OffsetTape2D a) where
    (OffsetTape2D t o) ?@ p = t ? (o + p)

instance (Ord a, Num a) => TotalIndex (V2 a) (OffsetTape2D a) where
    (OffsetTape2D t o) #@ p = t # (o + p)


