{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HaskellWorks.Data.MQuery where

import           Control.Monad
import qualified Data.DList                           as DL
import           GHC.Base
import           HaskellWorks.Data.Json.PartialValue
import           HaskellWorks.Data.Mini

newtype MQuery a = MQuery (DL.DList a)

deriving instance Functor     MQuery
deriving instance Applicative MQuery
deriving instance Monad       MQuery
deriving instance Alternative MQuery
deriving instance MonadPlus   MQuery

class AtLeastSize a where
  atLeastSize :: [a] -> Int -> Bool

instance AtLeastSize [a] where
  atLeastSize  _      0         = True
  atLeastSize (_:as)  n | n > 0 = atLeastSize as (n - 1)
  atLeastSize  _      _         = False

instance Show (MQuery JsonPartialValue) where
  showsPrec _ (MQuery das) = shows (Mini (Mini `fmap` das))

instance Show (MQuery String) where
  showsPrec _ (MQuery das) = case DL.toList das of
    as | as `atLeastSize` 100 -> ("[" ++) . showVs (take 100 as) . (", ..]" ++)
    []                        -> ("[]" ++)
    as                        -> ("[" ++) . showVs (take 100 as) . ("]" ++)
    where
      showVs :: [String] -> String -> String
      showVs (kv:kvs) = shows kv . foldl (.) id ((\jv -> (", " ++) . shows jv) `map` kvs)
      showVs []       = id

instance Show (MQuery (String, JsonPartialValue)) where
  showsPrec _ (MQuery das) = shows (Mini (Mini `fmap` das))

expandArray :: JsonPartialValue -> MQuery JsonPartialValue
expandArray jpv = case jpv of
  JsonPartialArray es -> MQuery $ DL.fromList es
  _                   -> MQuery   DL.empty

expandObject :: JsonPartialValue -> MQuery (String, JsonPartialValue)
expandObject jpv = case jpv of
  JsonPartialObject fs  -> MQuery $ DL.fromList fs
  _                     -> MQuery   DL.empty

selectField :: String -> (String, JsonPartialValue) -> MQuery JsonPartialValue
selectField fieldName (fieldName', jpv) | fieldName == fieldName' = MQuery $ DL.singleton jpv
selectField _         _                                           = MQuery   DL.empty

jsonKeys :: JsonPartialValue -> [String]
jsonKeys jpv = case jpv of
  JsonPartialObject fs  -> fst `map` fs
  _                     -> []

hasKey :: String -> JsonPartialValue -> Bool
hasKey fieldName jpv = fieldName `elem` jsonKeys jpv

inArray :: MQuery JsonPartialValue -> MQuery JsonPartialValue
inArray jpvs = jpvs >>= expandArray

jsonSize :: JsonPartialValue -> MQuery JsonPartialValue
jsonSize jpv = case jpv of
  JsonPartialArray  es  -> MQuery (DL.singleton (JsonPartialNumber (fromIntegral (length es))))
  JsonPartialObject es  -> MQuery (DL.singleton (JsonPartialNumber (fromIntegral (length es))))
  _                     -> MQuery (DL.singleton (JsonPartialNumber 0))
