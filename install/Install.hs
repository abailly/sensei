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

stackage_version = "lts-18.2"

docker_repository = "pankzsoft"

options =
  shakeOptions
    { shakeReport = ["report.html"],
      -- Generate a report that can be opened in a browser to analyze build
      shakeThreads = 0,
      -- Use all CPUs provided by the platform
      shakeFiles = "_build",
      -- Put Shake's database in directory '_build'
      shakeChange = ChangeDigest
      -- Rebuild only if content changes, not if date/time changes
    }

checkEnvironment :: Action ()
checkEnvironment = do
  creds <- getEnv "GOOGLE_APPLICATION_CREDENTIALS"
  case creds of
    Nothing -> throw (userError $ "GOOGLE_APPLICATION_CREDENTIALS environment variable is not set")
    Just _ -> pure ()

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
        needImage "pankzsoft/haskell-base"
        needDirectoryFiles
          "."
          [ "src//*.hs",
            "test//*.hs",
            "data//*.*",
            "app//*.hs",
            "package.yaml",
            "Dockerfile"
          ]
        need ["stack.yaml"]
        need ["ui/dist/index.html"]

  -- Build all images locally
  imageRule "pankzsoft/haskell-base" $ \img -> do
    need ["haskell-base/Dockerfile", "haskell-base/haskell.key"]
    cmd "docker" ["build", "--build-arg", "STACKAGE_VERSION=" <> stackage_version, "-t", toArg img, "haskell-base"]

  "build" ~> void (needImage (docker_repository </> "sensei"))

  "test" ~> do
    needHaskellSources
    cmd
      "docker"
      [ "run",
        "--rm",
        "-t",
        "-v",
        pwd <> ":/work",
        "-v",
        "haskell-stack:/home/user/.stack",
        "-w",
        "/work",
        "-e",
        "LOCAL_USER_ID=" <> uid,
        "pankzsoft/haskell-base",
        "stack",
        "test",
        "--allow-different-user",
        "--work-dir=.stack-work-build"
      ]

  imageRule (docker_repository </> "sensei") $ \img -> do
    need ["bin/sensei-exe"]
    cmd "docker" ["build", "-t", toArg img, "-f", "Dockerfile", "."]

  "bin/sensei-exe" %> \bin -> do
    needHaskellSources
    cmd
      "docker"
      [ "run",
        "--rm",
        "-t",
        "-v",
        pwd <> ":/work",
        "-v",
        "haskell-stack:/home/user/.stack",
        "-w",
        "/work",
        "-e",
        "LOCAL_USER_ID=" <> uid,
        "pankzsoft/haskell-base",
        "stack",
        "install",
        "--allow-different-user",
        "--work-dir=.stack-work-build",
        "--test",
        "--local-bin-path=bin"
      ]

  -- Tag local images with current SHA1 and push images
  -- to docker repo, iff git repository is clean
  "push" ~> do
    hd <- askGit (GitHead ())
    case hd of
      Just sha1 -> do
        void (needImage $ docker_repository </> "sensei")
        forM_ ["sensei", "haskell-base"] $ \img -> do
          cmd_ "docker" ["tag", docker_repository </> img <> ":latest", docker_repository </> img <> ":" <> sha1]
          cmd_ "docker" ["push", docker_repository </> img <> ":" <> sha1]
      Nothing -> putInfo "git working directory not clean, so not pushing anything"

  -- Configure the cloud environment to host the services
  -- Most of the work is delegated to terraform
  "deploy" ~> do
    checkEnvironment
    cmd_ (Cwd "infra") "terraform" ["plan", "-out=.deploy.plan", "-var-file=terraform.tfvars"]
    cmd_ (Cwd "infra") "terraform" ["apply", ".deploy.plan"]

  -- Clean up all resources set for the deployment
  "destroy" ~> do
    checkEnvironment
    cmd_ (Cwd "infra") "terraform" ["destroy", "-auto-approve", "-var-file=terraform.tfvars"]

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
    cmd
      "docker"
      [ "run",
        "--rm",
        "-t",
        "-v",
        (pwd </> "ui") <> ":/work",
        "-w",
        "/work",
        "-e",
        "LOCAL_USER_ID=" <> uid,
        "pankzsoft/haskell-base",
        "npm i && npm run build"
      ]

needDirectoryFiles dir patterns =
  need =<< getDirectoryFiles "" ((dir </>) <$> patterns)
