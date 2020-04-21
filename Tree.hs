module Tree where

import Data.List (sortOn)
import Data.String (fromString)

import Entry
import Util

data Tree = Tree [Entry]

instance GitObject Tree where
    gitData (Tree entries) =
        foldMap
            (\entry ->
                (fromString $ (entryMode entry) ++ " " ++ (entryName entry) ++ "\0") <>
                    (entryOid entry))
            (sortOn entryName entries)
    gitType _ = "tree"
