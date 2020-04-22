module Util.Hash
    ( ObjectId
    , IncrHash
    , oidFromBStr
    , sha1
    , hexDigest
    , bStrDigest
    , startHash
    , addToHash
    , finishHash
    ) where

import Crypto.Hash
import qualified Data.ByteArray as ByteArr
import qualified Data.ByteString.Lazy as BStr

type ObjectId = Digest SHA1

type IncrHash = Context SHA1

oidFromBStr :: BStr.ByteString -> ObjectId
oidFromBStr digest =
    case (digestFromByteString $ BStr.toStrict digest) of
        Just oid -> oid
        Nothing -> error "oidFromBStr called with the wrong length of digest"

sha1 :: BStr.ByteString -> ObjectId
sha1 = hashlazy

hexDigest :: ObjectId -> String
hexDigest = show

bStrDigest :: ObjectId -> BStr.ByteString
bStrDigest = BStr.pack . ByteArr.unpack

startHash :: IncrHash
startHash = hashInit

addToHash :: IncrHash -> BStr.ByteString -> IncrHash
addToHash i s = hashUpdates i $ BStr.toChunks s

finishHash :: IncrHash -> ObjectId
finishHash = hashFinalize
