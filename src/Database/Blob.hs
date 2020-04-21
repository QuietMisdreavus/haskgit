module Database.Blob where

import Data.ByteString.Lazy

import Util

-- a blob is a kind of `GitObject` that describes a file.
newtype Blob = Blob ByteString
    deriving (Show, Eq)

instance GitObject Blob where
    gitData (Blob b) = b
    gitType _ = "blob"
