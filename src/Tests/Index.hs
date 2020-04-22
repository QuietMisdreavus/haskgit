module Tests.Index where

import qualified Data.ByteString.Lazy as BStr
import Data.List (foldl')
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
    , TestLabel "replaces a file with a directory" replaceFileWithDir
    , TestLabel "replaces a directory with a file" replaceDirWithFile
    , TestLabel "reursively replaces a directory with a file" replaceDirWithFileRecursive
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
    return $ WriteIndex (Lockfile indexPath h) emptyIndexInner

buildIndex :: [String] -> IO Index
buildIndex entries = do
    initIdx <- mkIndex
    oid <- mkOid
    stat <- mkStat
    return $ foldl'
        (\i p -> fst $ addIndexEntry p oid stat i)
        initIdx
        entries

assertIndex :: Index -> [String] -> Assertion
assertIndex idx entries = assertEqual "" entries $ map entryPath $ indexEntries idx

addSingleFile :: Test
addSingleFile = TestCase $ do
    idx <- buildIndex ["alice.txt"]
    assertIndex idx ["alice.txt"]

replaceFileWithDir :: Test
replaceFileWithDir = TestCase $ do
    idx <- buildIndex
        [ "alice.txt"
        , "bob.txt"
        , "alice.txt/nested.txt"
        ]
    assertIndex idx
        [ "alice.txt/nested.txt"
        , "bob.txt"
        ]

replaceDirWithFile :: Test
replaceDirWithFile = TestCase $ do
    idx <- buildIndex
        [ "alice.txt"
        , "nested/bob.txt"
        , "nested"
        ]
    assertIndex idx
        [ "alice.txt"
        , "nested"
        ]

replaceDirWithFileRecursive :: Test
replaceDirWithFileRecursive = TestCase $ do
    idx <- buildIndex
        [ "alice.txt"
        , "nested/bob.txt"
        , "nested/inner/claire.txt"
        , "nested"
        ]
    assertIndex idx
        [ "alice.txt"
        , "nested"
        ]
