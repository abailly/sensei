#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , directory
             , filepath
             , shake
             , unix
-}

import Control.Exception (throw)
import Control.Monad (forM_, unless, when)
import Data.Functor
import Data.Maybe
import Development.Shake
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getCurrentDirectory, getXdgDirectory, removePathForcibly)
import System.Environment (getArgs, lookupEnv, withArgs)
import System.FilePath
import System.Posix.User (getRealUserID)

-- | Id of current user
type UID = String

-- | Get XDG bin directory, defaulting to ~/.local/bin
-- XDG_BIN_HOME is not part of the official XDG spec, but is a common convention
getXdgBinDir :: IO FilePath
getXdgBinDir = do
  maybeXdgBinHome <- lookupEnv "XDG_BIN_HOME"
  case maybeXdgBinHome of
    Just binDir -> pure binDir
    Nothing -> do
      -- XdgData gives us ~/.local/share, so we go up one level and add bin
      xdgDataDir <- getXdgDirectory XdgData ""
      pure $ takeDirectory xdgDataDir </> "bin"

options =
  shakeOptions
    { -- Generate a report that can be opened in a browser to analyze build
      shakeReport = ["report.html"],
      -- Use all CPUs provided by the platform
      shakeThreads = 0,
      -- Put Shake's database in directory '_build'
      shakeFiles = "_build",
      shakeVerbosity = Diagnostic
    }

install :: IO ()
install = do
  pwd <- getCurrentDirectory
  uid <- show . toInteger <$> getRealUserID
  putStrLn $ "Building sensei in directory " <> pwd <> " as user " <> uid
  args <- getArgs
  withArgs args $ runShake pwd uid

runShake :: FilePath -> UID -> IO ()
runShake pwd uid = shakeArgs options $ do
  let needHaskellSources = do
        needDirectoryFiles
          "."
          [ "src//*.hs",
            "test//*.hs",
            "data//*.*",
            "app//*.hs"
          ]
        need ["sensei.cabal", "cabal.project"]
        need ["ui/dist/index.html"]

      needClientSources =
        needDirectoryFiles
          "client"
          [ "*.c",
            "Makefile"
          ]

  -- Check if 'install' was requested
  args <- liftIO getArgs
  let wantInstall = "install" `elem` args

  if wantInstall
    then want ["install"]
    else want ["bin/sensei-exe"]

  "install" ~> do
    need ["bin/sensei-exe"]
    xdgBinDir <- liftIO getXdgBinDir
    liftIO $ createDirectoryIfMissing True xdgBinDir
    let targetExe = xdgBinDir </> "sensei-exe"
        targetLink = xdgBinDir </> "ep"
    putInfo $ "Installing sensei-exe to " <> targetExe
    -- Use install with explicit permissions (755 = rwxr-xr-x)
    cmd_ "install" ["-m", "755", "bin/sensei-exe", targetExe]
    putInfo $ "Creating symlink " <> targetLink <> " -> sensei-exe"
    -- Remove existing symlink if present
    liftIO $ removePathForcibly targetLink
    cmd_ [Cwd xdgBinDir] "ln" ["-sf", "sensei-exe", "ep"]
    putInfo $ "Installation complete. Make sure " <> xdgBinDir <> " is in your PATH."

  "client/senseic" %> \clientbin -> do
    needClientSources
    cmd "make" ["-C", "client"]

  "bin/sensei-exe" %> \bin -> do
    needHaskellSources
    cmd_ "cabal" ["build", "all"]
    cmd_ "cabal" ["test", "all"]
    Stdout exePath <- cmd "cabal" ["list-bin", "sensei-exe"]
    dirExists <- doesDirectoryExist "bin"
    unless dirExists $ cmd_ "mkdir" ["bin"]
    cmd_ "cp" [head (lines exePath), bin]

  "ui/dist/index.html" %> \_ -> do
    needDirectoryFiles
      "ui"
      [ "src//*.js",
        "src//*.html",
        "src//*.css",
        "package.json",
        "webpack.config.js",
        ".babelrc"
      ]
    command_
      [AddEnv "NODE_OPTIONS" "--no-experimental-webstorage", Cwd "ui"]
      "npm"
      ["i"]
    cmd
      [AddEnv "NODE_OPTIONS" "--no-experimental-webstorage", Cwd "ui"]
      "npm"
      ["run", "all"]

needDirectoryFiles dir patterns =
  need =<< getDirectoryFiles "" ((dir </>) <$> patterns)

main = install
