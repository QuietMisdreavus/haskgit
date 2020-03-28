module Entry where

import Data.ByteString.Lazy

data Entry = Entry {
    entryName :: String,
    entryOid :: ByteString
}
