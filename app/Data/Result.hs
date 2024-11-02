{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module Data.Result (Result(..)) where
import Geometry.Point

data Result = Result { point1    :: Point 2 Int
                     , point2    :: Point 2 Int
                     , parameter :: !Double}
            deriving Show

instance Semigroup Result where
  r1 <> r2 = if parameter r1 < parameter r2 then r1 else r2

instance Monoid Result where
  mempty = Result undefined undefined (recip 0)
