{-# LANGUAGE DataKinds #-}

module Main where
import Data.Result as R
import Geometry.Point
import Algorithms.Boundary.ImageBoundary
import Algorithms.Boundary.SortBoundary
import Algorithms.Boundary.Parameters
import Algorithms.Elongation
import Codec.Picture
import Options.Applicative
import System.FilePath

data What = ParameterSimplified | ParameterFull

data Options = Options
  { filename      :: FilePath
  , what          :: What
  , paramC        :: Double
  , boundaryFile  :: Maybe String
  , verbose       :: Bool
  }

-- FIXME: Do not allow lossy formats. Can it be better?
isLossy :: FilePath -> Bool
isLossy = flip elem [".jpg", ".jpeg"] . takeExtension

parameter :: What -> Double -> [Point 2 Int] -> Result Double
parameter ParameterSimplified = parameterSimplified
parameter ParameterFull       = parameterFull

maybeWriteBoundary :: Either String [Point 2 Int] -> Maybe String -> IO ()
maybeWriteBoundary (Right points) (Just name) = writePoints name points
maybeWriteBoundary _ _   = pure ()

processImage :: Options -> IO ()
processImage options = if isLossy name then
  putStrLn "Do not use lossy formats!" else do
  img <- readImage name
  let points = extractPoints img
  maybeWriteBoundary (snd <$> points) $ boundaryFile options
  case points >>= uncurry go of
    Left  failure -> putStrLn failure
    Right result  -> if (verbose options)
      then print result
      else putStrLn $ name ++ ", " ++ show (R.parameter result)
  where
    name    = filename options
    process = Main.parameter (what options) (paramC options)
    multiplyByCoeff = fmap . (*) . elongationCoeff . elongation
    go boundary blob = multiplyByCoeff blob <$> process <$> sortBoundary boundary

parser :: Parser Options
parser = Options
  <$> argument str (metavar "FILE")
  <*> (     flag' ParameterSimplified (long "simplified"
                                       <> help "Faster parameter")
        <|> flag' ParameterFull       (long "full"
                                       <> help "More informative parameter"))
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
