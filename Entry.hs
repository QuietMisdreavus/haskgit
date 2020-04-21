module Entry where

import Data.ByteString.Lazy
import System.Directory (Permissions, executable)

data Entry = Entry {
    entryName :: String,
    entryOid :: ByteString,
    entryStat :: Permissions
}

entryMode :: Entry -> String
entryMode e = if (executable $ entryStat e) then "100755" else "100644"
