module Commit where

import Prelude hiding (unlines)

import Data.ByteString.Lazy.Char8 (unlines)
import Data.Digest.Pure.SHA (showDigest)
import Data.String (fromString)

import Author
import Database (ObjectId)
import Util

data Commit = Commit {
    commitTree :: ObjectId,
    commitParent :: Maybe String,
    commitAuthor :: Author,
    commitMessage :: String
}

instance GitObject Commit where
    gitType _ = "commit"
    gitData commit =
        unlines $ [fromString "tree " <> (fromString $ showDigest $ commitTree commit)]
            ++ case (commitParent commit) of
                Just parent -> [fromString $ "parent " ++ parent]
                Nothing -> []
            ++ [fromString "author " <> (fromString $ renderAuthor $ commitAuthor commit),
            fromString "committer " <> (fromString $ renderAuthor $ commitAuthor commit),
            fromString "",
            fromString $ commitMessage commit]
