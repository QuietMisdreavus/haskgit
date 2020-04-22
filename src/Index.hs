module Index
    ( Index(..)
    , IndexInner(..)
    , emptyIndexInner
    , loadIndexToRead
    , loadIndexToWrite
    , indexEntries
    , indexHasEntry
    , addIndexEntry
    , releaseIndexLock
    , tryWriteIndex
    ) where

import Control.Monad (foldM)
import Data.Binary.Get
import qualified Data.ByteString.Builder as ByteBuf
import qualified Data.ByteString.Lazy as BStr
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

type IndexMap = Map.Map FilePath IndexEntry
type IndexParentsMap = Map.Map FilePath (Set.Set FilePath)

data Index
    = ReadOnlyIndex FilePath IndexInner
    | WriteIndex Lockfile IndexInner

data IndexInner = IndexInner
    { indexIMap :: IndexMap
    , indexIParents :: IndexParentsMap
    }

emptyIndexInner :: IndexInner
emptyIndexInner = IndexInner Map.empty Map.empty

loadIndexToWrite :: FilePath -> IO Index
loadIndexToWrite pathname = do
    lock <- mkBinLockfile pathname
    iMap <- readIndexInner pathname
    return $ WriteIndex lock iMap

loadIndexToRead :: FilePath -> IO Index
loadIndexToRead pathname = do
    iMap <- readIndexInner pathname
    return $ ReadOnlyIndex pathname iMap

addIndexEntry :: FilePath -> ObjectId -> FileStatus -> Index -> (Index, Bool)
-- indicies that were only opened for reading should not use this function
addIndexEntry _ _ _ (ReadOnlyIndex iPath iMap) = (ReadOnlyIndex iPath iMap, False)
addIndexEntry path oid stat (WriteIndex l inner) =
    let entry = mkIndexEntry path oid stat
    in (WriteIndex l $ addEntryToInner entry inner, True)

indexEntries :: Index -> [IndexEntry]
indexEntries i =
    let iMap = case i of
            (WriteIndex _ m) -> indexIMap m
            (ReadOnlyIndex _ m) -> indexIMap m
    in Map.elems iMap

indexHasEntry :: Index -> FilePath -> Bool
indexHasEntry i p =
    let inner = case i of
                (WriteIndex _ m) -> m
                (ReadOnlyIndex _ m) -> m
    in Map.member p (indexIMap inner) || Map.member p (indexIParents inner)

tryWriteIndex :: Bool -> Index -> IO ()
tryWriteIndex _ (ReadOnlyIndex _ _) =
    IOErr.ioError $ IOErr.userError "index was not opened for writing"
tryWriteIndex shouldWrite (WriteIndex lockfile iMap) =
    if shouldWrite
        then writeIndex (WriteIndex lockfile iMap)
        else rollbackLock lockfile

releaseIndexLock :: Index -> IO ()
releaseIndexLock (ReadOnlyIndex _ _) = return ()
releaseIndexLock (WriteIndex lock _) = rollbackLock lock

-- private functions

-- "load" helpers

readIndexInner :: FilePath -> IO IndexInner
readIndexInner pathname = do
    indexExists <- doesFileExist pathname
    if not indexExists
        then return emptyIndexInner
        else withBinaryFile pathname ReadMode (\h -> do
            let chkInit = mkChecksum h
            (chkHeader, count) <- loadIndexHeader chkInit
            (chkFinal, entries) <- loadEntries chkHeader (fromIntegral count)
            verifyChecksum chkFinal
            return $ loadIndexInner entries)

loadIndexHeader :: Checksum -> IO (Checksum, Word32)
loadIndexHeader chk = do
    (nextChk, buf) <- readToChecksum chk 12
    let (sig, vers, count) = runGet getHeader buf
    if sig /= fromString "DIRC"
        then IOErr.ioError $ IOErr.userError $
            "Signature: expected 'DIRC' but found '" ++ show sig ++ "'"
        else pure ()
    if vers /= 2
        then IOErr.ioError $ IOErr.userError $
            "Version: expected '2' but found '" ++ show vers ++ "'"
        else pure ()
    pure (nextChk, count)

getHeader :: Get (BStr.ByteString, Word32, Word32)
getHeader = (,,) <$> getLazyByteString 4 <*> getWord32be <*> getWord32be

loadIndexInner :: [IndexEntry] -> IndexInner
loadIndexInner = foldr addEntryToInner emptyIndexInner

loadEntries :: Checksum -> Int -> IO (Checksum, [IndexEntry])
loadEntries chk count =
    foldM
        (\(lastChk, entries) _ -> do
            (nextChk, e) <- loadEntry lastChk
            return (nextChk, entries <> [e]))
        (chk, [])
        [1..count]

loadEntry :: Checksum -> IO (Checksum, IndexEntry)
loadEntry chk = do
    bufInit <- readToChecksum chk 64
    (nextChk, buf) <- foldWhileM
        (\(_, b) -> BStr.last b /= 0)
        bufInit
        (\(lastChk, lastBuf) -> do
            (nextChk, buf) <- readToChecksum lastChk 8
            return (nextChk, lastBuf <> buf))
    return (nextChk, runGet getIndexEntry buf)

-- "add entry" helpers

addEntryToInner :: IndexEntry -> IndexInner -> IndexInner
addEntryToInner e IndexInner { indexIMap = iMap, indexIParents = pMap } =
    removeIndexConflicts e $ IndexInner
        { indexIMap = addEntryToMap e iMap
        , indexIParents = addEntryParents e pMap
        }

addEntryToMap :: IndexEntry -> IndexMap -> IndexMap
addEntryToMap e = Map.insert (entryPath e) e

addEntryParents :: IndexEntry -> IndexParentsMap -> IndexParentsMap
addEntryParents e pMap = foldr (addParent $ entryPath e) pMap $ entryParentDirs e

addParent :: FilePath -> FilePath -> IndexParentsMap -> IndexParentsMap
addParent v k pMap =
    let pSet = Map.findWithDefault Set.empty k pMap
    in Map.insert k (Set.insert v pSet) pMap

removeIndexConflicts :: IndexEntry -> IndexInner -> IndexInner
removeIndexConflicts e inner0 =
    let inner1 = foldr removeEntryAtPath inner0 $ entryParentDirs e;
        pSet = Map.findWithDefault Set.empty (entryPath e) (indexIParents inner1)
    in foldr removeEntryAtPath inner1 pSet

removeEntryAtPath :: FilePath -> IndexInner -> IndexInner
removeEntryAtPath pathname i =
    case Map.lookup pathname $ indexIMap i of
        Nothing -> i
        Just entry ->
            let iMap = Map.delete pathname $ indexIMap i;
                pMap = foldr
                    (removeEntryParent $ entryPath entry)
                    (indexIParents i)
                    (entryParentDirs entry)
            in IndexInner iMap pMap

removeEntryParent :: FilePath -> FilePath -> IndexParentsMap -> IndexParentsMap
removeEntryParent path dir pMap =
    let pSet = Map.findWithDefault Set.empty dir pMap;
        upSet = Set.delete path pSet
    in if Set.null upSet
        then Map.delete dir pMap
        else Map.insert dir upSet pMap

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
