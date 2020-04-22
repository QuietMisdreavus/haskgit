module Index
    ( Index(..)
    , IndexInner(..)
    , emptyIndexInner
    , loadIndexToRead
    , loadIndexToWrite
    , indexEntries
    , addIndexEntry
    , tryWriteIndex
    ) where

import Control.Monad (foldM)
import Data.Binary.Get
import qualified Data.ByteString.Builder as ByteBuf
import qualified Data.ByteString.Lazy as BStr
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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

type IndexMap = Map.Map String IndexEntry
type IndexParentsMap = Map.Map String (Set.Set String)

data Index
    = ReadOnlyIndex String IndexInner
    | WriteIndex Lockfile IndexInner

data IndexInner = IndexInner
    { indexIMap :: IndexMap
    , indexIParents :: IndexParentsMap
    }

emptyIndexInner :: IndexInner
emptyIndexInner = IndexInner Map.empty Map.empty

loadIndexInner :: [IndexEntry] -> IndexInner
loadIndexInner entries =
    foldr addEntryToInner emptyIndexInner entries

loadIndexToWrite :: String -> IO Index
loadIndexToWrite pathname = do
    lock <- mkBinLockfile pathname
    case lock of
        Left () -> IOErr.ioError $ IOErr.userError $ "Could not lock index at " ++ pathname
        Right l -> do
            iMap <- readIndexInner pathname
            return $ WriteIndex l iMap

loadIndexToRead :: String -> IO Index
loadIndexToRead pathname = do
    iMap <- readIndexInner pathname
    return $ ReadOnlyIndex pathname iMap

readIndexInner :: String -> IO IndexInner
readIndexInner pathname = do
    indexExists <- doesFileExist pathname
    if not indexExists
        then return emptyIndexInner
        else do
            withBinaryFile pathname ReadMode (\h -> do
                let chkInit = mkChecksum h
                (chkHeader, count) <- loadIndexHeader chkInit
                (chkFinal, entries) <- loadEntries chkHeader (fromIntegral count)
                verifyChecksum chkFinal
                return $ loadIndexInner entries)

addIndexEntry :: String -> ObjectId -> FileStatus -> Index -> (Index, Bool)
-- indicies that were only opened for reading should not use this function
addIndexEntry _ _ _ (ReadOnlyIndex iPath iMap) = (ReadOnlyIndex iPath iMap, False)
addIndexEntry path oid stat (WriteIndex l inner) =
    let entry = mkIndexEntry path oid stat
    in (WriteIndex l $ addEntryToInner entry inner, True)

addEntryToInner :: IndexEntry -> IndexInner -> IndexInner
addEntryToInner e (IndexInner { indexIMap = iMap, indexIParents = pMap }) =
    removeIndexConflicts e $ IndexInner
        { indexIMap = addEntryToMap e iMap
        , indexIParents = addEntryParents e pMap
        }

removeIndexConflicts :: IndexEntry -> IndexInner -> IndexInner
removeIndexConflicts e inner0 =
    let inner1 = foldr removeEntryAtPath inner0 $ entryParentDirs e;
        pSet = Map.findWithDefault Set.empty (entryPath e) (indexIParents inner1)
    in foldr removeEntryAtPath inner1 pSet

addEntryToMap :: IndexEntry -> IndexMap -> IndexMap
addEntryToMap e iMap = Map.insert (entryPath e) e iMap

addParent :: String -> String -> IndexParentsMap -> IndexParentsMap
addParent v k pMap =
    let pSet = Map.findWithDefault Set.empty k pMap
    in Map.insert k (Set.insert v pSet) pMap

addEntryParents :: IndexEntry -> IndexParentsMap -> IndexParentsMap
addEntryParents e pMap = foldr (addParent $ entryPath e) pMap $ entryParentDirs e

indexEntries :: Index -> [IndexEntry]
indexEntries i =
    let iMap = case i of
            (WriteIndex _ m) -> indexIMap m
            (ReadOnlyIndex _ m) -> indexIMap m
    in Map.elems iMap

removeEntryAtPath :: String -> IndexInner -> IndexInner
removeEntryAtPath pathname i =
    case (Map.lookup pathname $ indexIMap i) of
        Nothing -> i
        Just entry ->
            let iMap = Map.delete pathname $ indexIMap i;
                pMap = foldr
                    (removeEntryParent $ entryPath entry)
                    (indexIParents i)
                    (entryParentDirs entry)
            in IndexInner iMap pMap

removeEntryParent :: String -> String -> IndexParentsMap -> IndexParentsMap
removeEntryParent path dir pMap =
    let pSet = Map.findWithDefault Set.empty dir pMap;
        upSet = Set.delete path pSet
    in if Set.null upSet
        then Map.delete dir pMap
        else Map.insert dir upSet pMap

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
                , ByteBuf.word32BE $ fromIntegral $ Map.size $ indexIMap iMap]
    writeLockfileBStr lockfile header
    let hash = addToHash startHash header
    finalHash <- foldM
        (\h e -> do
            let b = renderEntry e
            writeLockfileBStr lockfile b
            pure $ addToHash h b)
        hash
        (indexIMap iMap)
    writeLockfileBStr lockfile $ bStrDigest $ finishHash finalHash
    commitLock lockfile

tryWriteIndex :: Bool -> Index -> IO ()
tryWriteIndex _ (ReadOnlyIndex _ _) =
    IOErr.ioError $ IOErr.userError "index was not opened for writing"
tryWriteIndex shouldWrite (WriteIndex lockfile iMap) =
    if shouldWrite
        then writeIndex (WriteIndex lockfile iMap)
        else rollbackLock lockfile
