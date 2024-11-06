{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Algorithms.Elongation (elongation, elongationCoeff) where
import Data.List
import Geometry.Point

sqr :: Num a => a -> a
sqr x = x ^ (2 :: Int)

mean :: [Point 2 Double] -> Point 2 Double
mean = fmap <$> scale <*> add where
  add = foldl1' $ liftA2 (+)
  scale = flip (/) . fromIntegral . length

subtractMean :: [Point 2 Double] -> [Point 2 Double]
subtractMean points = map subMean points where
  subMean   = liftA2 subtract meanPoint
  meanPoint = mean points

covariances :: [Point 2 Double] -> (Double, Double, Double)
covariances = foldl' sumCovariances (0, 0, 0) . subtractMean where
  sumCovariances (!dX, !dY, !cov) (Point2 x y) = (dX', dY', cov') where
    dX'  = dX  + sqr x
    dY'  = dY  + sqr y
    cov' = cov + x*y

eigenValues :: (Double, Double, Double) -> (Double, Double)
eigenValues (dX, dY, cov) = (l1, l2) where
  l1 = a + b
  l2 = a - b
  a = (dX + dY) / 2
  b = sqrt (sqr (dX - dY) + 4 * sqr cov) / 2

elongation :: [Point 2 Int] -> Double
elongation (eigenValues . covariances . map (fmap fromIntegral) -> (l1, l2)) =
  sqrt $ if e < 1 then e else recip e where
    e = abs $ l1 / l2

elongationCoeff :: Double -> Double
elongationCoeff e = pi * (3 * (1 + e) - sqrt ((3 * e + 1) * (e + 3))) / (4 * e)
