-- |Serves either statically embedded files or files from local
-- filesystem depending on the environment
module Sensei.UI where

import Network.Wai
import Network.Wai.Application.Static

-- | Serve static resources under `ui/dist` directory
staticResources :: Application
staticResources = staticApp (defaultFileServerSettings "ui/dist")
