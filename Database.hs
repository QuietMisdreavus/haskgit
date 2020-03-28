module Database where

import Prelude hiding (length)

import Codec.Compression.Zlib
import Data.ByteString.Lazy (ByteString, filter, length, hPut)
import Data.Digest.Pure.SHA
import Data.String (fromString)
import System.Directory
import System.FilePath
import System.IO (openTempFile, hClose)

import Util

type ObjectId = Digest SHA1State

data DatabaseObject = DatabaseObject {
    objectContent :: ByteString,
    objectId :: ObjectId
}

objectIdStr :: DatabaseObject -> String
objectIdStr obj = showDigest $ objectId obj

objectIdBStr :: DatabaseObject -> ByteString
objectIdBStr obj = bytestringDigest $ objectId obj

-- takes a `GitObject` and calculates its object ID.
mkObject :: (GitObject o) => o -> DatabaseObject
mkObject obj =
    let objData = gitData obj;
        content = (fromString $ (gitType obj)
                ++ " "
                ++ (show $ length $ objData)
                ++ "\0") <> objData
    in DatabaseObject content (sha1 $ content)

-- takes a ".git/objects" directory and a `DatabaseObject` and writes it to the database.
writeObject :: String -> DatabaseObject -> IO ()
writeObject path obj = do
    let strId = objectIdStr obj
    let objPath = path </> (take 2 strId) </> (drop 2 strId)
    let objDir = takeDirectory objPath
    createDirectoryIfMissing True objDir
    (tempPath, tempFile) <- openTempFile objDir "tmp_obj_"
    hPut tempFile $ compress $ objectContent obj
    hClose tempFile
    renameFile tempPath objPath
