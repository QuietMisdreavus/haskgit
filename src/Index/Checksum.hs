module Index.Checksum
    ( Checksum(..)
    , mkChecksum
    , readToChecksum
    , verifyChecksum
    ) where

import Data.ByteString.Lazy
import System.IO (Handle)
import System.IO.Error

import Util.Hash

data Checksum = Checksum Handle IncrHash

mkChecksum :: Handle -> Checksum
mkChecksum h = Checksum h startHash

readToChecksum :: Checksum -> Int -> IO (Checksum, ByteString)
readToChecksum (Checksum h hash) sz = do
    buf <- hGet h sz
    pure (Checksum h $ addToHash hash buf, buf)

verifyChecksum :: Checksum -> IO ()
verifyChecksum (Checksum h hash) = do
    buf <- hGet h 20
    if buf /= (bStrDigest $ finishHash hash)
    then
        let err = userError "Checksum does not match value stored on disk"
        in ioError $ ioeSetHandle err h
    else pure ()
