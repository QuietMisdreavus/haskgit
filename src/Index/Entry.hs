module Index.Entry where

import Data.Bits
import Data.ByteString.Builder
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BStr
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word32)
import System.Posix.Files
import System.Posix.Types

import Util.Hash (ObjectId, bStrDigest)

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
}

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
        pad = BStr.pack $ take (fromIntegral $ 8 - (bufLen `mod` 8)) $ repeat 0
    in buf <> pad
