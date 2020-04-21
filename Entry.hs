module Entry where

import System.Directory (Permissions, executable)

import Database (ObjectId)

data Entry = Entry {
    entryName :: String,
    entryId :: ObjectId,
    entryStat :: Permissions
} deriving (Show)

entryMode :: Entry -> String
entryMode e = if (executable $ entryStat e) then "100755" else "100644"
