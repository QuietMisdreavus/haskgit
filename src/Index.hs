module Index where

import Control.Monad (foldM)
import qualified Data.ByteString.Builder as ByteBuf
import qualified Data.Map.Strict as Map
import qualified System.IO.Error as IOErr
import System.Posix.Files (FileStatus)

import Util.Hash
import Lockfile
import Index.Entry

data Index = Index Lockfile (Map.Map String IndexEntry)

mkIndex :: String -> IO Index
mkIndex pathname = do
    lock <- mkBinLockfile pathname
    case lock of
        Left () -> IOErr.ioError $ IOErr.userError $ "Could not lock index at " ++ pathname
        Right l -> pure $ Index l Map.empty

addIndexEntry :: String -> ObjectId -> FileStatus -> Index -> Index
addIndexEntry path oid stat (Index l iMap) =
    let entry = mkIndexEntry path oid stat
    in Index l $ Map.insert path entry iMap

writeIndex :: Index -> IO ()
writeIndex (Index lockfile iMap) = do
    let header = ByteBuf.toLazyByteString $
            mconcat
                [ ByteBuf.stringUtf8 "DIRC"
                , ByteBuf.word32BE 2
                , ByteBuf.word32BE $ fromIntegral $ Map.size iMap]
    writeLockfileBStr lockfile header
    let hash = addToHash startHash header
    finalHash <- foldM
        (\h e -> do
            let b = renderEntry e
            writeLockfileBStr lockfile b
            pure $ addToHash h b)
        hash
        iMap
    writeLockfileBStr lockfile $ bStrDigest $ finishHash finalHash
    commitLock lockfile
