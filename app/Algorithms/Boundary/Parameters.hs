{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Algorithms.Boundary.Parameters (
  parameter1,
  parameter2) where
import Algorithms.Geometry.SSSP
import Geometry.Point
import Geometry.Polygon
import Data.Result
import Data.Ext
import Data.List
import Data.Maybe
import Data.PlaneGraph as PG
import Algorithms.Geometry.PolygonTriangulation.Types
import Geometry.PlanarSubdivision
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VV

-- Parameter #1
dist :: Point 2 Int -> Point 2 Int -> Double
dist p1 p2 = sqrt $ fromIntegral $ squaredEuclideanDist p1 p2

pairwiseDist :: [Point 2 Int] -> [Double]
pairwiseDist [] = error "List is too short"
pairwiseDist whole@(_:xs) = zipWith dist whole $ xs

perimeter :: [Point 2 Int] -> Double
perimeter [] = error "List is too short"
perimeter xs = foldl' (+) 0 (pairwiseDist xs) + dist (head xs) (last xs)

minimizeOnHead :: Double -> Result -> Double -> [Double] -> [Point 2 Int] -> Result
minimizeOnHead _ _ _ _ [] = error "List is too short"
minimizeOnHead c score p curveDists (pHead:pTail) =
  foldl' go score $ zip pTail curveDists where
    go acc (x, cd) = if 2 * l < c * p then acc else
      acc <> Result pHead x (dist x pHead / l) where
      l = min cd (p - cd)
      
parameter1 :: Double -> [Point 2 Int] -> Result
parameter1 c points = go mempty curveDists points where
  go acc [] _ = acc
  go acc dists@(dHead:dTail) ps@(_:psTail) =
    go (minimizeOnHead c acc p dists ps) (map (subtract dHead) dTail) psTail
  p = perimeter points
  curveDists = tail $ scanl' (+) 0 $ pairwiseDist points

-- Parameter #2
triangles ::
  VV.Vector (Point 2 Int) -> PlaneGraph s Int PolygonEdgeType PolygonFaceData Rational
triangles = triangulate . unsafeFromVector . VV.map (ext . fmap toRational)

distInside :: VV.Vector (Point 2 Int) -> V.Vector Int -> Int -> Double
distInside points distMap idx = go 0 $ idx where
  go acc n = if v == n then acc else
    go (acc + dist (points VV.! n) (points VV.! v)) v where
    v = distMap V.! n

-- Minimize the parameter for points in the range from idx to len-1
minimizeN :: Double
  -> Int
  -> VV.Vector (Point 2 Int)
  -> PlaneGraph s Int PolygonEdgeType PolygonFaceData Rational
  -> Double -> Result -> Result
minimizeN c idx points g p acc = go acc 0 idx where
  distMap    = sssp' idx g
  len        = VV.length points
  startPoint = points VV.! idx
  go acc' distBoundary i = if i == len - 1 then acc' else
    let prevPoint = points VV.! i
        curPoint  = points VV.! (i + 1)
        distBoundary' = distBoundary + dist prevPoint curPoint
        l = min distBoundary' (p - distBoundary')
        r = distInside points distMap $ i + 1
        update = if 2 * l < c * p then mempty else Result startPoint curPoint (r/l) in
      go (acc' <> update) distBoundary' $ i + 1

parameter2Full :: Double
  -> VV.Vector (Point 2 Int)
  -> PlaneGraph s Int PolygonEdgeType PolygonFaceData Rational
  -> Double
  -> Result
parameter2Full c points g p = go mempty 0 where
  len = VV.length points
  go acc n = if n == len - 1 then acc else
    go (minimizeN c n points g p acc) $ n + 1

parameter2 :: Double -> [Point 2 Int] -> Result
parameter2 c points = case parameter1 c points of
  result@(Result p1 p2 _) -> if directlyVisible then result else
    parameter2Full c points' g p where
    directlyVisible = sssp' p1Idx g V.! p2Idx == p1Idx
    points' = VV.fromList points
    g = triangles points'
    p = perimeter points
    p1Idx = fromJust $ VV.elemIndex p1 points'
    p2Idx = fromJust $ VV.elemIndex p2 points'
