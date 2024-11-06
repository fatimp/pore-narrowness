{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Result (Result(..)) where
import Geometry.Point

data Result a = Result { point1    :: Point 2 Int
                       , point2    :: Point 2 Int
                       , parameter :: !a}
            deriving (Show, Functor)

instance Ord a => Semigroup (Result a) where
  r1 <> r2 = if parameter r1 < parameter r2 then r1 else r2

instance (Num a, Ord a, Fractional a) => Monoid (Result a) where
  mempty = Result undefined undefined (recip 0)
