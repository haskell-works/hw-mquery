{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HaskellWorks.Data.MQuery where

import           Control.Monad
import qualified Data.DList                           as DL
import           GHC.Base
import           HaskellWorks.Data.AtLeastSize
import           HaskellWorks.Data.Json.PartialValue
import           HaskellWorks.Data.Mini
import           HaskellWorks.Data.Shows

newtype MQuery a = MQuery (DL.DList a)

deriving instance Functor     MQuery
deriving instance Applicative MQuery
deriving instance Monad       MQuery
deriving instance Alternative MQuery
deriving instance MonadPlus   MQuery

instance Show (MQuery JsonPartialValue) where
  showsPrec _ (MQuery das) = shows (Mini das)

instance Show (MQuery String) where
  showsPrec _ (MQuery das) = case DL.toList das of
    as | as `atLeastSize` 100 -> ("[" ++) . showsVs (take 100 as) . (", ..]" ++)
    []                        -> ("[]" ++)
    as                        -> ("[" ++) . showsVs (take 100 as) . ("]" ++)

instance Show (MQuery (String, JsonPartialValue)) where
  showsPrec _ (MQuery das) = shows (Mini das)

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
