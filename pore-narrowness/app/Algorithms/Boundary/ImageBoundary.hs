{-# LANGUAGE DataKinds #-}

module Algorithms.Boundary.ImageBoundary (extractPoints, writePoints) where
import System.IO
import Control.Monad
import Codec.Picture
import Codec.Picture.Types
import Geometry.Point

isBoundary :: Pixel8 -> Pixel8 -> Pixel8 -> Pixel8 -> Pixel8 -> Bool
isBoundary c l r b t = (c /= 0) && ((l == 0) || (r == 0) || (b == 0) || (t == 0))

isBoundaryPixel :: Image Pixel8 -> Int -> Int -> Bool
isBoundaryPixel img x y = isBoundary c l r b t where
  c = pixelAt img x y
  l = pixelAt img (x-1) y
  r = pixelAt img (x+1) y
  b = pixelAt img x (y-1)
  t = pixelAt img x (y+1)

isBlobPixel :: Image Pixel8 -> Int -> Int -> Bool
isBlobPixel img x y = pixelAt img x y /= 0

filterPixels :: (Image Pixel8 -> Int -> Int -> Bool) -> Image Pixel8 -> [Point 2 Int]
filterPixels f img = filter go indices where
  indices = Point2 <$> [1..w-2] <*> [1..h-2]
  w = imageWidth  img
  h = imageHeight img
  go (Point2 x y) = f img x y

forceGrayscale :: DynamicImage -> Either String (Image Pixel8)
forceGrayscale (ImageY8   img) = Right img
forceGrayscale (ImageRGB8 img) = Right $ pixelMap computeLuma img
forceGrayscale _ = Left "Not a grayscale 8 bit image"

extractPoints :: Either String DynamicImage -> Either String ([Point 2 Int], [Point 2 Int])
extractPoints = (=<<) $ fmap flt . forceGrayscale where
  flt = (,) <$> filterPixels isBoundaryPixel <*> filterPixels isBlobPixel

writePoints :: String -> [Point 2 Int] -> IO ()
writePoints name points = withFile name WriteMode $ \handle ->
  forM_ points $ \(Point2 x y) -> hPutStrLn handle $ show x ++ ", " ++ show y
