{-# LANGUAGE TemplateHaskell #-}
-- |Serves either statically embedded files or files from local
-- filesystem depending on the environment
module Sensei.Server.UI where

import Network.Wai
import Network.Wai.Application.Static
import Sensei.Server.Config
import qualified Data.ByteString as BS
import Data.FileEmbed

uiFiles :: [(FilePath, BS.ByteString)]
uiFiles = $(makeRelativeToProject "ui/dist" >>= embedDir)

-- | Serve static resources under `ui/dist` directory
userInterface :: Maybe Env -> Application
userInterface (Just Prod) = embeddedFiles uiFiles
userInterface _ = staticApp (defaultFileServerSettings "ui/dist")

embeddedFiles ::
  [(FilePath, BS.ByteString)] -> Application
embeddedFiles = staticApp . embeddedSettings
