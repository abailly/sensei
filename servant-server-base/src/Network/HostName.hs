-- | Compute hostname using FFI
-- stolen from https://hackage.haskell.org/package/hostname-1.0/docs/src/Network-HostName.html
module Network.HostName (
    HostName, getHostName
  ) where

import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Array

type HostName = String

foreign import ccall unsafe "gethostname" gethostname :: CString -> CSize -> IO CInt

getHostName :: IO HostName
getHostName = allocaArray0 size $ \cstr -> do
        throwErrnoIfMinus1_ "getHostName" $ gethostname cstr (fromIntegral size)
        peekCString cstr
    where size = 256

