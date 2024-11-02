{-# LANGUAGE DataKinds #-}

module Main where
import Data.Result
import Algorithms.Boundary.ImageBoundary
import Algorithms.Boundary.SortBoundary
import Algorithms.Boundary.Parameters
import Codec.Picture
import System.Environment

processImage :: String -> IO (Either String (String, Result))
processImage name = do
  img <- readImage name
  pure $ process <$> (extractBoundary img >>= sortBoundary) where
    process points = param `seq` (name, param) where
      param = parameter2 0.1 points

reportParameters :: Either String (String, Result) -> IO ()
reportParameters (Left str) = putStrLn str
reportParameters (Right (name, res)) = case res of
  Result _ _ p -> putStrLn $ name ++ ", " ++ show p

main :: IO ()
main = do
  args <- getArgs
  case args of
    [nameIn] -> processImage nameIn >>= reportParameters
    _        -> error "Usage: shit-score image.png"
