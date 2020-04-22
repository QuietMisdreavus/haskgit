module Entry where

import Data.Word (Word32)
import System.Directory
import qualified System.Posix.Files as U

import qualified Index.Entry as I
import Util.Hash (ObjectId)

data Entry = Entry {
    entryName :: String,
    entryId :: ObjectId,
    entryStat :: Permissions
} deriving (Show)

entryMode :: Entry -> String
entryMode e = if (executable $ entryStat e) then "100755" else "100644"

entryFromIndex :: I.IndexEntry -> Entry
entryFromIndex e = Entry
    { entryName = I.entryPath e
    , entryId = I.entryId e
    , entryStat = permFromStat $ I.entryMode e
    }

permFromStat :: Word32 -> Permissions
permFromStat m =
    if m == 0o100755
        then setOwnerExecutable True emptyPermissions
        else emptyPermissions
