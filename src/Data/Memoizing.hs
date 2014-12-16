{-# LANGUAGE TemplateHaskell #-}

module Data.Memoizing where

import Data.Function.Memoize
import Linear.V2

deriveMemoizable ''V2
