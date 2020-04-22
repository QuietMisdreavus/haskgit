module Index.Entry
    ( IndexEntry(..)
    , mkIndexEntry
    , entryParentDirs
    , entryFileName
    , renderEntry
    , getIndexEntry
    ) where

import Control.Monad (replicateM)
import Data.Binary.Get
import Data.Bits
import Data.ByteString.Builder
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BStr
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List (inits)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word32)
import System.FilePath
import System.Posix.Files
import System.Posix.Types

import Util.Hash (ObjectId, bStrDigest, oidFromBStr)

entryModeValue :: FileStatus -> Word32
entryModeValue perm =
    if ((popCount (fileMode perm .&. ownerExecuteMode)) > 0)
        then 0o100755
        else 0o100644

timeToNS :: POSIXTime -> Word32
timeToNS t =
    let sec = t - (fromInteger $ truncate t);
        nano = sec * 1000000000
    in truncate nano

data IndexEntry = IndexEntry {
    entryCTime :: EpochTime,
    entryCTimeNS :: Word32,
    entryMTime :: EpochTime,
    entryMTimeNS :: Word32,
    entryDevice :: DeviceID,
    entryInode :: FileID,
    entryMode :: Word32,
    entryUid :: UserID,
    entryGid :: GroupID,
    entrySize :: FileOffset,
    entryId :: ObjectId,
    entryFlags :: Word32,
    entryPath :: String
} deriving (Show, Eq)

mkIndexEntry :: String -> ObjectId -> FileStatus -> IndexEntry
mkIndexEntry path oid stat =
    IndexEntry {
        entryCTime = statusChangeTime stat,
        entryCTimeNS = timeToNS $ statusChangeTimeHiRes stat,
        entryMTime = modificationTime stat,
        entryMTimeNS = timeToNS $ modificationTimeHiRes stat,
        entryDevice = deviceID stat,
        entryInode = fileID stat,
        entryMode = entryModeValue stat,
        entryUid = fileOwner stat,
        entryGid = fileGroup stat,
        entrySize = fileSize stat,
        entryId = oid,
        entryFlags = fromIntegral $ min (length path) 0xFFF,
        entryPath = path
    }

entryParentDirs :: IndexEntry -> [String]
entryParentDirs e =
    filter (/= ".")
        $ map joinPath
        $ tail
        $ inits
        $ splitDirectories
        $ dropFileName
        $ entryPath e

entryFileName :: IndexEntry -> String
entryFileName e = takeFileName $ entryPath e

renderEntry :: IndexEntry -> ByteString
renderEntry entry =
    let statFields = [
            fromIntegral . fromEnum . entryCTime,
            fromIntegral . entryCTimeNS,
            fromIntegral . fromEnum . entryMTime,
            fromIntegral . entryMTimeNS,
            fromIntegral . entryDevice,
            fromIntegral . entryInode,
            fromIntegral . entryMode,
            fromIntegral . entryUid,
            fromIntegral . entryGid,
            fromIntegral . entrySize] <*> [entry];
        stats = foldMap (word32BE) statFields;
        buf = (toLazyByteString $ mconcat [
            stats,
            lazyByteString $ bStrDigest $ entryId entry,
            word16BE $ fromIntegral $ entryFlags entry,
            stringUtf8 $ entryPath entry]) <> BStr.pack [0];
        bufLen = BStr.length buf;
        padLen = 8 - (bufLen `mod` 8);
        pad = BStr.pack $
            take (fromIntegral $ if padLen == 8 then 0 else padLen) $
            repeat 0
    in buf <> pad

getIndexEntry :: Get IndexEntry
getIndexEntry = do
    (ctime:ctimeNS:mtime:mtimeNS:dev:ino:mode:uid:gid:size:[]) <-
        replicateM 10 getWord32be
    oid <- getLazyByteString 20
    flags <- getWord16be
    path <- getLazyByteStringNul
    return IndexEntry {
        entryCTime = fromIntegral ctime,
        entryCTimeNS = ctimeNS,
        entryMTime = fromIntegral mtime,
        entryMTimeNS = mtimeNS,
        entryDevice = fromIntegral dev,
        entryInode = fromIntegral ino,
        entryMode = mode,
        entryUid = fromIntegral uid,
        entryGid = fromIntegral gid,
        entrySize = fromIntegral size,
        entryId = oidFromBStr oid,
        entryFlags = fromIntegral flags,
        entryPath = unpack path}
