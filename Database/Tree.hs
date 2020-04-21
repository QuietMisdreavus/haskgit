module Database.Tree where

import Data.Digest.Pure.SHA (bytestringDigest)
import Data.List (sortOn, foldl')
import Data.List.Extra (dropSuffix)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import System.FilePath (splitPath, pathSeparator)

import Database
import Entry
import Util

-- represents a directory of blobs or subtrees
data Tree = Tree (Map.Map String TreeEntry)
    deriving (Show)

-- records in a tree can be a blob or another tree
data TreeEntry = SubTree Tree | SubEntry Entry
    deriving (Show)

-- create an empty Tree.
emptyTree :: Tree
emptyTree = Tree Map.empty

-- build up a tree from a list of entries. entries with names like "foo/bar" will be
-- organized into subtrees named "foo/" to ensure proper sorting.
buildTree :: [Entry] -> Tree
buildTree es =
    let entries = sortOn entryName es
    in foldl'
        (\t e ->
            let p = splitPath $ entryName e;
                path = init p;
                name = last p
            in addEntryToTree t path name e)
        emptyTree
        entries

-- @addEntryToTree tree path name entry@ sorts the given entry into the proper subtree,
-- based on the given path. @path@ should be a list of directory names leading up to the
-- filename.
addEntryToTree :: Tree -> [String] -> String -> Entry -> Tree
addEntryToTree (Tree tree) [] name entry =
    Tree $ Map.insert name (SubEntry entry) tree
addEntryToTree (Tree tree) path name entry =
    let (k:newPath) = path;
        st = fromMaybe emptyTree $ getSubTree k (Tree tree);
        st' = addEntryToTree st newPath name entry
    in Tree $ Map.insert k (SubTree st') tree

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

data TreeObjectEntry = SubTreeObject TreeObject | SubEntryObject Entry

-- the file mode for a 'TreeObjectEntry', used to render trees to disk.
treeEntryMode :: TreeObjectEntry -> String
treeEntryMode (SubTreeObject _) = "40000"
treeEntryMode (SubEntryObject e) = entryMode e

instance GitObject TreeObject where
    gitType _ = "tree"
    gitData (TreeObject tree) =
        Map.foldMapWithKey
            (\name (oid, e) ->
                let n = dropSuffix [pathSeparator] name;
                    mode = treeEntryMode e
                in (fromString $ mode ++ " " ++ n ++ "\0") <> (bytestringDigest oid))
            tree
