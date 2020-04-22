module Tests.Index where

import qualified Data.ByteString.Lazy as BStr
import qualified Data.Map.Strict as Map
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Posix.Files
import System.Random
import Test.HUnit

import Index
import Index.Entry
import Lockfile
import Util.Hash

indexTests :: Test
indexTests = TestList
    [ TestLabel "adds a single file" addSingleFile
    ]

mkOid :: IO ObjectId
mkOid = do
    buf <- take 20 <$> randoms <$> getStdGen
    return $ oidFromBStr $ BStr.pack buf

mkTmpPath :: IO String
mkTmpPath = canonicalizePath "tmp"

mkIndexPath :: IO String
mkIndexPath = (</> "index") <$> mkTmpPath

mkStat :: IO FileStatus
mkStat = getFileStatus =<< getExecutablePath

fakeHandle :: IO Handle
fakeHandle = do
    h <- openFile "/dev/null" ReadWriteMode
    hClose h -- close the handle to crash if writes are attempted
    return h

mkIndex :: IO Index
mkIndex = do
    h <- fakeHandle
    indexPath <- mkIndexPath
    return $ WriteIndex (Lockfile indexPath h) Map.empty

addSingleFile :: Test
addSingleFile = TestCase $ do
    idx <- mkIndex
    oid <- mkOid
    stat <- mkStat
    let (finalIdx, _) = addIndexEntry "alice.txt" oid stat idx
    assertEqual "adding a single file" ["alice.txt"] $ map entryPath $ indexEntries finalIdx
