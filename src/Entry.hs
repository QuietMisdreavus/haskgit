module Entry
    ( Entry(..)
    , entryMode
    ) where

import System.Directory

import Util.Hash (ObjectId)

data Entry = Entry {
    entryName :: FilePath,
    entryId :: ObjectId,
    entryStat :: Permissions
} deriving (Show)

entryMode :: Entry -> String
entryMode e = if (executable $ entryStat e) then "100755" else "100644"
