{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Algorithms.Boundary.SortBoundary (sortBoundary, Point2D(..)) where
import Geometry.Point
import Data.BKTree as BK
import qualified Data.Set as S
import Data.List as L
import Data.Monoid

firstJusts :: [Maybe a] -> Maybe a
firstJusts xs = getFirst $ foldMap First xs

newtype Point2D a = Point2D { getPoint :: Point 2 a } deriving (Eq, Ord, Show)

instance Metric (Point2D Int) where
  distance p1 p2 = getPoint p1 `dist` getPoint p2 where
    dist (Point2 x1 y1) (Point2 x2 y2) = abs (x1 - x2) `max` abs (y1 - y2)

maybeSortBoundary' :: [Point2D Int] -> Maybe [Point2D Int]
maybeSortBoundary' [] = error "Called on empty list"
maybeSortBoundary' whole@(x:xs) = go [x] S.empty (fromList xs) 1 where
  total = length whole
  go :: [Point2D Int] -> S.Set (Point2D Int) -> BKTree (Point2D Int) -> Int -> Maybe [Point2D Int]
  go [] _ _ _ = error "Never happens"
  go sorted@(sHead:sTail) neighborsSeen unsorted n = if n == total
    then Just sorted
    else if any (\point -> distance point sHead /= 1) neighborsSeen
      then Nothing
      else let neighbors = elemsDistance 1 sHead unsorted
               neighborsSeen' = if L.null sTail
                                then S.empty
                                else S.union neighborsSeen $ S.fromList neighbors
               go' neighbor = go
                              (neighbor:sorted)
                              (S.delete neighbor neighborsSeen')
                              (BK.delete neighbor unsorted)
                              (n + 1)
               paths = map go' neighbors
           in firstJusts paths

maybeSortBoundary :: [Point 2 Int] -> Maybe [Point 2 Int]
maybeSortBoundary points = map getPoint <$> (maybeSortBoundary' $ map Point2D points)

sortBoundary :: [Point 2 Int] -> Either String [Point 2 Int]
sortBoundary points = case maybeSortBoundary points of
  Nothing -> Left "The pore has an invalid boundary"
  Just x  -> Right x
