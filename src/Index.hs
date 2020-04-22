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

data Index
    = ReadOnlyIndex String (Map.Map String IndexEntry)
    | WriteIndex Lockfile (Map.Map String IndexEntry)

loadIndexToWrite :: String -> IO Index
loadIndexToWrite pathname = do
    lock <- mkBinLockfile pathname
    case lock of
        Left () -> IOErr.ioError $ IOErr.userError $ "Could not lock index at " ++ pathname
        Right l -> do
            iMap <- readIndexMap pathname
            return $ WriteIndex l iMap

loadIndexToRead :: String -> IO Index
loadIndexToRead pathname = do
    iMap <- readIndexMap pathname
    return $ ReadOnlyIndex pathname iMap

readIndexMap :: String -> IO (Map.Map String IndexEntry)
readIndexMap pathname = do
    indexExists <- doesFileExist pathname
    if not indexExists
        then return Map.empty
        else do
            withBinaryFile pathname ReadMode (\h -> do
                let chkInit = mkChecksum h
                (chkHeader, count) <- loadIndexHeader chkInit
                (chkFinal, entries) <- loadEntries chkHeader (fromIntegral count)
                verifyChecksum chkFinal
                return $ Map.fromList $ map (\e -> (entryPath e, e)) entries)

addIndexEntry :: String -> ObjectId -> FileStatus -> Index -> (Index, Bool)
-- indicies that were only opened for reading should not use this function
addIndexEntry _ _ _ (ReadOnlyIndex iPath iMap) = (ReadOnlyIndex iPath iMap, False)
addIndexEntry path oid stat (WriteIndex l iMap) =
    let entry = mkIndexEntry path oid stat;
        updated = case (Map.lookup path iMap) of
            Nothing -> True
            Just prevEntry -> prevEntry /= entry
    in (WriteIndex l $ Map.insert path entry iMap, updated)

indexEntries :: Index -> [IndexEntry]
indexEntries i =
    let iMap = case i of
            (WriteIndex _ m) -> m
            (ReadOnlyIndex _ m) -> m
    in Map.elems iMap

getHeader :: Get (BStr.ByteString, Word32, Word32)
getHeader = (,,) <$> getLazyByteString 4 <*> getWord32be <*> getWord32be

loadIndexHeader :: Checksum -> IO (Checksum, Word32)
loadIndexHeader chk = do
    (nextChk, buf) <- readToChecksum chk 12
    let (sig, vers, count) = runGet getHeader buf
    if sig /= (fromString "DIRC")
        then IOErr.ioError $ IOErr.userError $
            "Signature: expected 'DIRC' but found '" ++ (show sig) ++ "'"
        else pure ()
    if vers /= 2
        then IOErr.ioError $ IOErr.userError $
            "Version: expected '2' but found '" ++ (show vers) ++ "'"
        else pure ()
    pure (nextChk, count)

loadEntry :: Checksum -> IO (Checksum, IndexEntry)
loadEntry chk = do
    bufInit <- readToChecksum chk 64
    (nextChk, buf) <- foldWhileM
        (\(_, b) -> (BStr.last b) /= 0)
        bufInit
        (\(lastChk, lastBuf) -> do
            (nextChk, buf) <- readToChecksum lastChk 8
            return (nextChk, lastBuf <> buf))
    return (nextChk, runGet getIndexEntry buf)

loadEntries :: Checksum -> Int -> IO (Checksum, [IndexEntry])
loadEntries chk count =
    foldM
        (\(lastChk, entries) _ -> do
            (nextChk, e) <- loadEntry lastChk
            return (nextChk, entries <> [e]))
        (chk, [])
        [1..count]

writeIndex :: Index -> IO ()
writeIndex (ReadOnlyIndex _ _) =
    IOErr.ioError $ IOErr.userError "index was not opened for writing"
writeIndex (WriteIndex lockfile iMap) = do
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
tryWriteIndex _ (ReadOnlyIndex _ _) =
    IOErr.ioError $ IOErr.userError "index was not opened for writing"
tryWriteIndex shouldWrite (WriteIndex lockfile iMap) =
    if shouldWrite
        then writeIndex (WriteIndex lockfile iMap)
        else rollbackLock lockfile
