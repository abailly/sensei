{-# LANGUAGE ViewPatterns #-}

module Sensei.CLI.Terminal where

import qualified Control.Exception.Safe as Exc
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Preface.Utils (decodeUtf8')
import System.Console.ANSI
import System.Directory (removeFile)
import System.Environment
import System.FilePath ((<.>))
import System.IO
import System.Posix.Temp (mkstemp)
import System.Process
  ( CreateProcess (std_err, std_in, std_out),
    StdStream (Inherit),
    createProcess,
    proc,
    waitForProcess,
  )

captureNote :: IO Text.Text
captureNote =
  lookupEnv "EDITOR" >>= maybe captureInTerminal captureInEditor

captureInEditor :: String -> IO Text.Text
captureInEditor (words -> (exe : args)) = do
  (fp, hdl) <- mkstemp "capture-"
  let editFile = fp <.> "md" -- let editor try to be smart
  hClose hdl
  (_, _, _, h) <-
    createProcess
      (proc exe (args <> [editFile]))
        { std_in = Inherit,
          std_out = Inherit,
          std_err = Inherit
        }
  void $ waitForProcess h
  decodeUtf8' <$> BS.readFile editFile <* removeFile editFile
captureInEditor editor =
  error $ "Expected an executable and some arguments, got :'" <> editor <> "'"

captureInTerminal :: IO Text.Text
captureInTerminal = do
  setSGR
    [ SetConsoleIntensity NormalIntensity,
      SetColor Foreground Vivid White,
      SetColor Background Dull Blue
    ]
  putStr "Record a note, type Ctrl+D when done"
  setSGR [Reset]
  putStrLn ""
  capture "" []
  where
    capture :: Text -> [Text.Text] -> IO Text.Text
    capture curLine acc = do
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      eof <- isEOF
      if eof
        then pure $ Text.unlines (reverse acc)
        else do
          input <- Exc.try $ getKey stdin
          case input of
            Left (_ex :: Exc.IOException) -> pure $ Text.unlines (reverse acc)
            Right "\EOT" -> pure $ Text.unlines (reverse $ curLine : acc)
            Right "\ESC[A" ->
              -- move up
              capture curLine acc
            Right "\ESC[B" ->
              -- move down
              capture curLine acc
            Right "\ESC[C" ->
              -- move right
              capture curLine acc
            Right "\ESC[D" ->
              -- move left
              capture "" (curLine : acc)
            Right "\BS" ->
              capture curLine acc
            Right "\DEL" -> do
              let newline = Text.dropEnd 1 curLine
              hPutStr stdout "\ESC[1000D"
              hPutStr stdout "\ESC[0K"
              hPutStr stdout (Text.unpack newline)
              capture newline acc
            Right "\n" -> hPutStr stdout "\n\ESC[1E" >> capture "" (curLine : acc)
            Right "\r" -> hPutStr stdout "\n\ESC[1E" >> capture "" (curLine : acc)
            Right other -> hPutStr stdout other >> capture (curLine <> Text.pack other) acc

getKey :: Handle -> IO [Char]
getKey hdl = reverse <$> getKey' ""
  where
    getKey' chars = do
      char <- getChar
      more <- hReady hdl
      (if more then getKey' else return) (char : chars)

readPassword :: IO Text
readPassword = do
  putStr "Enter password: "
  Text.pack <$> Exc.bracket_ (hSetEcho stdin False) (hSetEcho stdin True) getLine
