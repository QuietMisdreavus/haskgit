module Index where

import Control.Monad (foldM)
import Data.Binary.Get
import qualified Data.ByteString.Builder as ByteBuf
import qualified Data.ByteString.Lazy as BStr
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import Data.Word
import System.Directory
import System.IO
import qualified System.IO.Error as IOErr
import System.Posix.Files (FileStatus)

import Util
import Util.Hash
import Lockfile
import Index.Checksum
import Index.Entry

data Index = Index Lockfile (Map.Map String IndexEntry)

mkIndex :: String -> IO Index
mkIndex pathname = do
    lock <- mkBinLockfile pathname
    case lock of
        Left () -> IOErr.ioError $ IOErr.userError $ "Could not lock index at " ++ pathname
        Right l -> pure $ Index l Map.empty

addIndexEntry :: String -> ObjectId -> FileStatus -> Index -> (Index, Bool)
addIndexEntry path oid stat (Index l iMap) =
    let entry = mkIndexEntry path oid stat;
        updated = case (Map.lookup path iMap) of
            Nothing -> True
            Just prevEntry -> (entryId prevEntry) /= oid
    in (Index l $ Map.insert path entry iMap, updated)

loadIndex :: Index -> IO Index
loadIndex (Index lock _) = do
    let (Lockfile filename _) = lock
    indexExists <- doesFileExist filename
    if not indexExists
        then return $ Index lock Map.empty
        else do
            withBinaryFile filename ReadMode (\h -> do
                let chkInit = mkChecksum h
                (chkHeader, count) <- loadIndexHeader chkInit
                (chkFinal, entries) <- loadEntries chkHeader (fromIntegral count)
                verifyChecksum chkFinal
                return $ Index lock $ Map.fromList $ map (\e -> (entryPath e, e)) entries)

getHeader :: Get (BStr.ByteString, Word32, Word32)
getHeader = (,,) <$> getLazyByteString 4 <*> getWord32be <*> getWord32be

loadIndexHeader :: Checksum -> IO (Checksum, Word32)
loadIndexHeader chk = do
    (sum, buf) <- readToChecksum chk 12
    let (sig, vers, count) = runGet getHeader buf
    if sig /= (fromString "DIRC")
        then IOErr.ioError $ IOErr.userError $
            "Signature: expected 'DIRC' but found '" ++ (show sig) ++ "'"
        else pure ()
    if vers /= 2
        then IOErr.ioError $ IOErr.userError $
            "Version: expected '2' but found '" ++ (show vers) ++ "'"
        else pure ()
    pure (sum, count)

loadEntry :: Checksum -> IO (Checksum, IndexEntry)
loadEntry chk = do
    bufInit <- readToChecksum chk 64
    (sum, buf) <- foldWhileM
        (\(_, b) -> (BStr.last b) /= 0)
        bufInit
        (\(lastChk, lastBuf) -> do
            (nextChk, buf) <- readToChecksum lastChk 8
            return (nextChk, lastBuf <> buf))
    return (sum, runGet getIndexEntry buf)

loadEntries :: Checksum -> Int -> IO (Checksum, [IndexEntry])
loadEntries chk count =
    foldM
        (\(lastChk, entries) _ -> do
            (nextChk, e) <- loadEntry lastChk
            return (nextChk, entries <> [e]))
        (chk, [])
        [1..count]

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

tryWriteIndex :: Bool -> Index -> IO ()
tryWriteIndex shouldWrite i =
    if shouldWrite
        then writeIndex i
        else let (Index lockfile _) = i in rollbackLock lockfile
