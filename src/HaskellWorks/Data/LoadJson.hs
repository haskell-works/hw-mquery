{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.LoadJson where

import           Control.Monad
import qualified Data.ByteString                                  as BS
import qualified Data.Vector.Storable                             as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Decode
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Json.PartialValue
import           HaskellWorks.Data.Json.Succinct.Cursor
import           HaskellWorks.Data.Json.Succinct.Index
import           HaskellWorks.Data.Json.Succinct.PartialIndex
import           HaskellWorks.Data.Json.Value
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Diagnostics.Time

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

loadJsonPartial :: String -> IO JsonPartialValue
loadJsonPartial filename = do
  !cursor <- readJson filename
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult
