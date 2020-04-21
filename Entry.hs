module Entry where

import Data.ByteString.Lazy
import Data.Digest.Pure.SHA (bytestringDigest)
import System.Directory (Permissions, executable)

import Database (ObjectId)

data Entry = Entry {
    entryName :: String,
    entryId :: ObjectId,
    entryStat :: Permissions
} deriving (Show)

entryOid :: Entry -> ByteString
entryOid e = bytestringDigest $ entryId e

entryMode :: Entry -> String
entryMode e = if (executable $ entryStat e) then "100755" else "100644"
