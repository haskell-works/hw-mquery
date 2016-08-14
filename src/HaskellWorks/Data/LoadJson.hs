{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.LoadJson where

import           Control.Monad
import qualified Data.ByteString                                          as BS
import qualified Data.ByteString.Internal                                 as BSI
import qualified Data.Vector.Storable                                     as DVS
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
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.L0
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.L1
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.L2
import           HaskellWorks.Data.Succinct.BalancedParens.RangeMinMax.L3
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.CsPoppy
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512S
import           HaskellWorks.Diagnostics.Time
import           System.IO
import           System.IO.MMap

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

loadJsonRawWithIndex :: String -> IO (BS.ByteString, DVS.Vector Word64, DVS.Vector Word64)
loadJsonRawWithIndex filename = do
  jsonFr    <- mmapFileForeignPtr filename ReadOnly Nothing
  jsonIbFr  <- mmapFileForeignPtr (filename ++ ".ib") ReadOnly Nothing
  jsonBpFr  <- mmapFileForeignPtr (filename ++ ".bp") ReadOnly Nothing
  let jsonBS  = fromForeignRegion jsonFr    :: BS.ByteString
  let jsonIb  = fromForeignRegion jsonIbFr  :: DVS.Vector Word64
  let jsonBp  = fromForeignRegion jsonBpFr  :: DVS.Vector Word64
  return (jsonBS, jsonIb, jsonBp)

loadJsonWithIndex :: String -> IO JsonPartialValue
loadJsonWithIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let cursor = JsonCursor jsonBS (BitShown jsonIb) (SimpleBalancedParens jsonBp) 1
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

loadJsonWithPoppy512Index :: String -> IO JsonPartialValue
loadJsonWithPoppy512Index filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let cursor = JsonCursor jsonBS (makePoppy512 jsonIb) (SimpleBalancedParens jsonBp) 1
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

loadJsonWithPoppy512SIndex :: String -> IO JsonPartialValue
loadJsonWithPoppy512SIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let cursor = JsonCursor jsonBS (makePoppy512S jsonIb) (SimpleBalancedParens jsonBp) 1
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

loadJsonWithPoppy512SMinMaxIndex :: String -> IO JsonPartialValue
loadJsonWithPoppy512SMinMaxIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let rangeMinMax = mkRangeMinMaxL3 jsonBp
  let !a  = rangeMinMaxL3Base    rangeMinMax
  let !a1 = rangeMinMaxL3Min     rangeMinMax
  let !a2 = rangeMinMaxL3Max     rangeMinMax
  let !a3 = rangeMinMaxL3Excess  rangeMinMax
  let !b  = rangeMinMaxL2Base    a
  let !b1 = rangeMinMaxL2Min     a
  let !b2 = rangeMinMaxL2Max     a
  let !b3 = rangeMinMaxL2Excess  a
  let !c  = rangeMinMaxL1Base    b
  let !c1 = rangeMinMaxL1Min     b
  let !c2 = rangeMinMaxL1Max     b
  let !c3 = rangeMinMaxL1Excess  b
  let !_  = rangeMinMaxSimple    c
  let !d1 = rangeMinMaxL0Min     c
  let !d2 = rangeMinMaxL0Max     c
  let !d3 = rangeMinMaxL0Excess  c
  let !_  = a1 DVS.! 0
  let !_  = a2 DVS.! 0
  let !_  = a3 DVS.! 0
  let !_  = b1 DVS.! 0
  let !_  = b2 DVS.! 0
  let !_  = b3 DVS.! 0
  let !_  = c1 DVS.! 0
  let !_  = c2 DVS.! 0
  let !_  = c3 DVS.! 0
  let !_  = d1 DVS.! 0
  let !_  = d2 DVS.! 0
  let !_  = d3 DVS.! 0
  let cursor = JsonCursor jsonBS (makePoppy512S jsonIb) rangeMinMax 1
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

loadJsonWithCsPoppyIndex :: String -> IO JsonPartialValue
loadJsonWithCsPoppyIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let cursor = JsonCursor jsonBS (makeCsPoppy jsonIb) (SimpleBalancedParens jsonBp) 1
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

loadJsonWithPoppy512Index2 :: String -> IO JsonPartialValue
loadJsonWithPoppy512Index2 filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let cursor = JsonCursor jsonBS (makePoppy512 jsonIb) (SimpleBalancedParens (makePoppy512 jsonBp)) 1
                :: JsonCursor BSI.ByteString Poppy512 (SimpleBalancedParens Poppy512)
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

loadJsonWithPoppy512SIndex2 :: String -> IO JsonPartialValue
loadJsonWithPoppy512SIndex2 filename = do
  (jsonBS, jsonIb, jsonBp) <- loadJsonRawWithIndex filename
  let cursor = JsonCursor jsonBS (makePoppy512S jsonIb) (SimpleBalancedParens (makePoppy512S jsonBp)) 1
                :: JsonCursor BSI.ByteString Poppy512S (SimpleBalancedParens Poppy512S)
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
