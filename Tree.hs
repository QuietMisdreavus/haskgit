module Tree where

import Data.List (sortOn)
import Data.String (fromString)

import Entry
import Util

entryMode :: String
entryMode = "100644"

data Tree = Tree [Entry]

instance GitObject Tree where
    gitData (Tree entries) =
        foldMap
            (\entry ->
                (fromString $ entryMode ++ " " ++ (entryName entry) ++ "\0") <>
                    (entryOid entry))
            (sortOn entryName entries)
    gitType _ = "tree"
