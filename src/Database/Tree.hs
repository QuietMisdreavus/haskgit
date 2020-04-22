module Database.Tree where

import Data.List (sortOn, foldl')
import Data.List.Extra (dropSuffix)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Numeric (showOct)
import System.Directory
import System.FilePath
    ( splitPath
    , pathSeparator
    , takeFileName
    , addTrailingPathSeparator
    , dropTrailingPathSeparator
    )

import Database
import Index.Entry
import Util
import Util.Hash

-- represents a directory of blobs or subtrees
data Tree = Tree (Map.Map String TreeEntry)
    deriving (Show)

-- records in a tree can be a blob or another tree
data TreeEntry = SubTree Tree | SubEntry IndexEntry
    deriving (Show)

-- create an empty Tree.
emptyTree :: Tree
emptyTree = Tree Map.empty

-- build up a tree from a list of entries. entries with names like "foo/bar" will be
-- organized into subtrees named "foo/" to ensure proper sorting.
buildTree :: [IndexEntry] -> Tree
buildTree es =
    foldl'
        (\t e ->
            let path = entryParentDirs e
            in addEntryToTree t path e)
        emptyTree
        es

-- @addEntryToTree tree path entry@ sorts the given entry into the proper subtree,
-- based on the given path. @path@ should be a list of directory names leading up to the
-- filename.
addEntryToTree :: Tree -> [String] -> IndexEntry -> Tree
addEntryToTree (Tree tree) [] entry =
    Tree $ Map.insert (entryFileName entry) (SubEntry entry) tree
addEntryToTree (Tree tree) path entry =
    let (k:newPath) = path;
        key = addTrailingPathSeparator $ takeFileName k;
        st = fromMaybe emptyTree $ getSubTree key (Tree tree);
        st' = addEntryToTree st newPath entry
    in Tree $ Map.insert key (SubTree st') tree

-- tries to lookup a subtree with the given name. if an existing record is actually a blob
-- instead of a tree, this function will return 'Nothing'.
getSubTree :: String -> Tree -> Maybe Tree
getSubTree k (Tree tree) =
    case (Map.lookup k tree) of
        Just (SubTree t) -> Just t
        _ -> Nothing

-- renders the given tree into a 'TreeObject', executing the given action on subtrees in a
-- depth-first search.
traverseTree :: (DatabaseObject -> IO ()) -> Tree -> IO (ObjectId, TreeObject)
traverseTree f (Tree tree) = do
    tree' <- traverse
        (\e -> case e of
            SubEntry entry -> pure $ ((entryId entry), SubEntryObject entry)
            SubTree st -> do
                (oid, sto) <- traverseTree f st
                pure $ (oid, SubTreeObject sto))
        tree
    let treeobj = TreeObject tree'
    let obj = mkObject treeobj
    f obj
    pure ((objectId obj), treeobj)

-- a 'Tree' that has had 'ObjectId's calculated for all its subtrees.
data TreeObject = TreeObject (Map.Map String (ObjectId, TreeObjectEntry))

data TreeObjectEntry = SubTreeObject TreeObject | SubEntryObject IndexEntry

-- the file mode for a 'TreeObjectEntry', used to render trees to disk.
treeEntryMode :: TreeObjectEntry -> String
treeEntryMode (SubTreeObject _) = "40000"
treeEntryMode (SubEntryObject e) = showOct (entryMode e) ""

instance GitObject TreeObject where
    gitType _ = "tree"
    gitData (TreeObject tree) =
        Map.foldMapWithKey
            (\name (oid, e) ->
                let n = dropTrailingPathSeparator name;
                    mode = treeEntryMode e
                in (fromString $ mode ++ " " ++ n ++ "\0") <> (bStrDigest oid))
            tree
