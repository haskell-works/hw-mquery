{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.LoadJson where

import           Control.Monad
import qualified Data.ByteString                                  as BS
import qualified Data.ByteString.Internal                         as BSI
import qualified Data.Vector.Storable                             as DVS
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Decode
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Json.PartialValue
import           HaskellWorks.Data.Json.Succinct.Cursor
import           HaskellWorks.Data.Json.Succinct.Index
import           HaskellWorks.Data.Json.Succinct.PartialIndex
import           HaskellWorks.Data.Json.Value
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Diagnostics.Time
import           System.IO
import           System.IO.MMap

type ForeignRegion = (ForeignPtr Word8, Int, Int)

-- | Write out a vector verbatim into an open file handle.
hPutVector :: forall a. Storable a => Handle -> DVS.Vector a -> IO ()
hPutVector h v = withForeignPtr fp $ \p -> hPutBuf h (p `plusPtr` offset) sz
      where
        (fp, offset, n) = DVS.unsafeToForeignPtr v
        eltsize = sizeOf (undefined :: a)
        sz = n * eltsize

-- | Write the vector verbatim to a file.
writeVector :: forall a. Storable a => FilePath -> DVS.Vector a -> IO ()
writeVector fp v = withFile fp WriteMode $ \h -> hPutVector h v

readJson :: String -> IO (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
readJson path = do
  bs <- BS.readFile path
  putStrLn "Read file"
  !cursor <- measure (fromByteString bs :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
  putStrLn "Created cursor"
  return cursor

loadJson :: String -> IO (Either DecodeError [JsonValue])
loadJson filename = do
  !cursor <- readJson filename
  let !jsonResult = (jsonIndexAt >=> jsonValueAt) cursor
  return $ (:[]) `fmap` jsonResult

loadByteString :: FilePath -> IO BS.ByteString
loadByteString filepath = do
  (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr filepath ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  return bs

instance FromForeignRegion BS.ByteString where
  fromForeignRegion (fptr, offset, size) = BSI.fromForeignPtr (castForeignPtr fptr) offset size

instance FromForeignRegion (DVS.Vector Word64) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

loadJsonWithIndex :: String -> IO JsonPartialValue
loadJsonWithIndex filename = do
  jsonFr    <- mmapFileForeignPtr filename ReadOnly Nothing
  jsonIbFr  <- mmapFileForeignPtr (filename ++ ".ib") ReadOnly Nothing
  jsonBpFr  <- mmapFileForeignPtr (filename ++ ".bp") ReadOnly Nothing
  let jsonBS  = fromForeignRegion jsonFr    :: BS.ByteString
  let jsonIb  = fromForeignRegion jsonIbFr  :: DVS.Vector Word64
  let jsonBp  = fromForeignRegion jsonBpFr  :: DVS.Vector Word64
  let cursor = JsonCursor jsonBS (BitShown jsonIb) (SimpleBalancedParens jsonBp) 1
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

indexJson :: String -> IO ()
indexJson filename = do
  JsonCursor _ (BitShown ib) (SimpleBalancedParens bp) _ <- readJson filename
  let wib = DVS.unsafeCast ib :: DVS.Vector Word8
  let wbp = DVS.unsafeCast bp :: DVS.Vector Word8
  writeVector (filename ++ ".ib") wib
  writeVector (filename ++ ".bp") wbp

loadJsonPartial :: String -> IO JsonPartialValue
loadJsonPartial filename = do
  !cursor <- readJson filename
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult
