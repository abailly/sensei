import Network.CORS (WithCORS (..))
import Network.Wai.Application.Static
import Preface.Log
import Preface.Server

main :: IO ()
main = do
  withAppServer config app waitServer
  where
    config = AppServerConfig { serverAssignedName = "localhost:8000", cors =  NoCORS, listenPort = 8000 }
    app = \logger -> logInfo logger ("Starting server" :: String) >> pure (staticApp $ defaultFileServerSettings ".")
