#!/usr/bin/env cabal
{- cabal:
build-depends: base
             , directory
             , filepath
             , shake
             , unix
-}

import Control.Exception (throw)
import Control.Monad (forM_, unless)
import Data.Functor
import Data.Maybe
import Development.Shake
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, lookupEnv, withArgs)
import System.FilePath
import System.Posix.User (getRealUserID)

-- | Id of current user
type UID = String

options =
    shakeOptions
        { shakeReport = ["report.html"]
        , -- Generate a report that can be opened in a browser to analyze build
          shakeThreads = 0
        , -- Use all CPUs provided by the platform
          shakeFiles = "_build"
        , -- Put Shake's database in directory '_build'
          shakeChange = ChangeDigest
          -- Rebuild only if content changes, not if date/time changes
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
                [ "src//*.hs"
                , "test//*.hs"
                , "data//*.*"
                , "app//*.hs"
                , "package.yaml"
                ]
            need ["sensei.cabal", "cabal.project"]
            need ["ui/dist/index.html"]

        needClientSources =
            needDirectoryFiles
                "client"
                [ "*.c"
                , "Makefile"
                ]

    "build" ~> void (need ["bin/sensei-exe"])

    "client/senseic" %> \clientbin -> do
        needClientSources
        cmd "make" ["-C", "client"]

    "bin/sensei-exe" %> \bin -> do
        needHaskellSources
        cmd_ "cabal" ["build", "exe:sensei-exe"]
        Stdout exePath <- cmd "cabal" ["list-bin", "sensei-exe"]
        dirExists <- doesDirectoryExist "bin"
        unless dirExists $ cmd_ "mkdir" ["bin"]
        cmd_ "cp" [head (lines exePath), "bin"]

    "ui/dist/index.html" %> \_ -> do
        needDirectoryFiles
            "ui"
            [ "src//*.js"
            , "src//*.html"
            , "src//*.css"
            , "package.json"
            , "webpack.config.js"
            , ".babelrc"
            ]
        cmd_ (Cwd "ui") "npm" ["i"]
        cmd (Cwd "ui") "npm" ["run", "all"]

needDirectoryFiles dir patterns =
    need =<< getDirectoryFiles "" ((dir </>) <$> patterns)

main = install
