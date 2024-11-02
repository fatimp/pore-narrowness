{-# LANGUAGE DataKinds #-}

module Main where
import Data.Result
import Geometry.Point
import Algorithms.Boundary.ImageBoundary
import Algorithms.Boundary.SortBoundary
import Algorithms.Boundary.Parameters
import Codec.Picture
import Options.Applicative

data What = Parameter1 | Parameter2

data Options = Options
  { filename      :: FilePath
  , what          :: What
  , paramC        :: Double
  , writeBoundary :: Maybe String
  , verbose       :: Bool
  }

parameter :: What -> Double -> [Point 2 Int] -> Result
parameter Parameter1 = parameter1
parameter Parameter2 = parameter2

processImage :: Options -> IO (Either String (String, Result))
processImage options = do
  img <- readImage name
  pure $ process <$> (extractBoundary img >>= sortBoundary) where
    name  = filename options
    process points = result `seq` (name, result) where
      result = Main.parameter (what options) (paramC options) points

reportParameters :: Bool -> Either String (FilePath, Result) -> IO ()
reportParameters _         (Left failure) = putStrLn failure
reportParameters isVerbose (Right (name, res)) =
  if isVerbose then print res else
    case res of
      (Result _ _ p) -> putStrLn $ name ++ ", " ++ show p

parser :: Parser Options
parser = Options
  <$> argument str (metavar "FILE")
  <*> (     flag' Parameter1 (long "p1" <> help "Faster parameter")
        <|> flag' Parameter2 (long "p2" <> help "Slower parameter"))
  <*> option auto (long "param"
                   <> short 'c'
                   <> value 0
                   <> metavar "PARAM"
                   <> help "Parameter c")
  <*> optional (strOption (long "boundary"
                           <> metavar "CSV"
                           <> help "Write boundary to a CSV file"))
  <*> switch (long "verbose" <> short 'v' <> help "Write additional information")

parseArguments :: IO Options
parseArguments = execParser $ info (parser <**> helper) mempty

main :: IO ()
main = parseArguments >>= processOptions where
  processOptions options = processImage options >>= (reportParameters $ verbose options)
