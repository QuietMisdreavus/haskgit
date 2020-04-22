module Database.Commit
    ( Commit(..)
    ) where

import Prelude hiding (unlines)

import Data.ByteString.Lazy.Char8 (unlines)
import Data.String (fromString)

import Database.Author
import Util
import Util.Hash

data Commit = Commit {
    commitTree :: ObjectId,
    commitParent :: Maybe String,
    commitAuthor :: Author,
    commitMessage :: String
}

instance GitObject Commit where
    gitType _ = "commit"
    gitData commit =
        unlines $ [fromString ("tree " <> hexDigest (commitTree commit))]
            ++ case commitParent commit of
                Just parent -> [fromString $ "parent " ++ parent]
                Nothing -> []
            ++ [fromString ("author " <> renderAuthor (commitAuthor commit)),
            fromString ("committer " <> renderAuthor (commitAuthor commit)),
            fromString "",
            fromString $ commitMessage commit]
