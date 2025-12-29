module Main where

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Sensei.Bsky.Leaflet.Markdown (mkMarkdownDocument)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import System.IO (hPutStrLn, stderr, stdin)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      content <- TIO.hGetContents stdin
      convert content
    [filePath] -> do
      content <- TIO.readFile filePath
      convert content
    _ -> do
      progName <- getProgName
      die $ "Usage: " <> progName <> " [markdown-file]\n\nIf no file is specified, reads from stdin."

convert :: Text -> IO ()
convert content = do
  result <- mkMarkdownDocument content
  case result of
    Left err -> do
      hPutStrLn stderr $ "Error parsing markdown: " <> err
      die "Conversion failed"
    Right document -> do
      LBS.putStrLn $ encodePretty document
