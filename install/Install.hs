module Install where

import Control.Exception (throw)
import Control.Monad (forM_)
import Data.Functor
import Data.Maybe
import Development.Shake
import Shake.Extra.Docker
import Shake.Extra.Git
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, lookupEnv, withArgs)
import System.FilePath
import System.Posix.User (getRealUserID)

-- | Id of current user
type UID = String

stackage_version = "lts-18.13"

docker_repository = "pankzsoft"

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
    askGit <- gitHeadRule
    addBuiltinImageRule
    addBuiltinContainerRule

    let needHaskellSources = do
            needDirectoryFiles
                "."
                [ "src//*.hs"
                , "test//*.hs"
                , "data//*.*"
                , "app//*.hs"
                , "package.yaml"
                ]
            need ["stack.yaml", "sensei.cabal", "cabal.project" ]
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
        cmd
            "stack"
            [ "install"
            , "--allow-different-user"
            , "--work-dir=.stack-work-build"
            , "--test"
            , "--local-bin-path=bin"
            ]

    -- Tag local images with current SHA1 and push images
    -- to docker repo, iff git repository is clean
    "push" ~> do
        hd <- askGit (GitHead ())
        case hd of
            Just sha1 -> do
                need ["bin/sensei-exe"]
                cmd_ "git" ["push", "origin", sha1 <> ":deploy"]
            Nothing -> putInfo "git working directory not clean, so not pushing anything"

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
