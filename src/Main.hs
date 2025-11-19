module Main (main) where

import qualified Data.Text.IO as T
import           Aster.Parser
import           Aster.Renderer
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath (takeDirectory, (</>))
import           Text.Megaparsec (errorBundlePretty)

inputDir :: FilePath
inputDir = "aster-files"

inputFile :: FilePath
inputFile = inputDir </> "index.aster"

outputFile :: FilePath
outputFile = inputDir </> "index.html"

main :: IO ()
main = do
  input <- T.readFile inputFile
  case parseAster input of
    Left err -> putStrLn (errorBundlePretty err)
    Right nodes -> do
      let html = renderDocument nodes
      createDirectoryIfMissing True (takeDirectory outputFile)
      writeFile outputFile html
      putStrLn $ "Generated " ++ outputFile
