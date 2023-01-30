import Network.CORS (WithCORS (..))
import Network.Wai.Application.Static
import Preface.Log
import Preface.Server

main :: IO ()
main = do
  withAppServer "localhost:8000" NoCORS 8000 app waitServer
  where
    app = \logger -> logInfo logger ("Starting server" :: String) >> pure (staticApp $ defaultFileServerSettings ".")
