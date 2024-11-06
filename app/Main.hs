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
  , boundaryFile  :: Maybe String
  , verbose       :: Bool
  }

parameter :: What -> Double -> [Point 2 Int] -> Result Double
parameter Parameter1 = parameter1
parameter Parameter2 = parameter2

maybeWriteBoundary :: Either String [Point 2 Int] -> Maybe String -> IO ()
maybeWriteBoundary (Right points) (Just name) = writeBoundary name points
maybeWriteBoundary _ _   = pure ()

processImage :: Options -> IO ()
processImage options = do
  img <- readImage name
  let boundary = extractBoundary img >>= sortBoundary
  maybeWriteBoundary boundary $ boundaryFile options
  case process <$> boundary of
    Left  failure               -> putStrLn failure
    Right result@(Result _ _ p) -> if (verbose options)
      then print result
      else putStrLn $ name ++ ", " ++ show p
  where
    name    = filename options
    process = Main.parameter (what options) (paramC options)

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
main = parseArguments >>= processImage
