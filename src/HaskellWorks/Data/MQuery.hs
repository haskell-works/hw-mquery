{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HaskellWorks.Data.MQuery where

import           Control.Monad
import           Data.List
import qualified Data.DList                           as DL
import           GHC.Base
import           HaskellWorks.Data.Json.PartialValue
import           HaskellWorks.Data.Row
import           HaskellWorks.Data.ToBool
import           Text.PrettyPrint.ANSI.Leijen

newtype MQuery a = MQuery (DL.DList a)

deriving instance Functor     MQuery
deriving instance Applicative MQuery
deriving instance Monad       MQuery
deriving instance Alternative MQuery
deriving instance MonadPlus   MQuery

mQuery :: MQuery a -> DL.DList a
mQuery (MQuery a) = a

instance ToBool (MQuery a) where
  toBool = toBool . mQuery

instance Pretty (MQuery JsonPartialValue) where
  pretty = pretty . Row 120 . mQuery

instance Pretty (MQuery String) where
  pretty x = prettyRowOfString (Row 120 (mQuery x))

instance Pretty (MQuery (String, JsonPartialValue)) where
  pretty (MQuery das) = pretty (Row 120 das)

hasKV :: String -> JsonPartialValue -> JsonPartialValue -> MQuery JsonPartialValue
hasKV k v (JsonPartialObject xs)  = if (k, v) `elem` xs then MQuery (DL.singleton (JsonPartialObject xs)) else MQuery DL.empty
hasKV _ _  _                      = MQuery DL.empty

item :: JsonPartialValue -> MQuery JsonPartialValue
item jpv = case jpv of
  JsonPartialArray es -> MQuery $ DL.fromList es
  _                   -> MQuery   DL.empty

entry :: JsonPartialValue -> MQuery (String, JsonPartialValue)
entry jpv = case jpv of
  JsonPartialObject fs  -> MQuery $ DL.fromList fs
  _                     -> MQuery   DL.empty

named :: String -> (String, JsonPartialValue) -> MQuery JsonPartialValue
named fieldName (fieldName', jpv) | fieldName == fieldName' = MQuery $ DL.singleton jpv
named _         _                                           = MQuery   DL.empty

key :: (String, JsonPartialValue) -> MQuery String
key (k, _) = MQuery $ DL.singleton k

dlTake :: Int -> DL.DList a -> DL.DList a
dlTake n = DL.fromList . take n . DL.toList

select :: ToBool b => a -> (a -> b) -> MQuery a
select a f = if toBool (f a) then MQuery (DL.singleton a) else MQuery DL.empty

jsonKeys :: JsonPartialValue -> [String]
jsonKeys jpv = case jpv of
  JsonPartialObject fs  -> fst `map` fs
  _                     -> []

hasKey :: String -> JsonPartialValue -> Bool
hasKey fieldName jpv = fieldName `elem` jsonKeys jpv

jsonSize :: JsonPartialValue -> MQuery JsonPartialValue
jsonSize jpv = case jpv of
  JsonPartialArray  es  -> MQuery (DL.singleton (JsonPartialNumber (fromIntegral (length es))))
  JsonPartialObject es  -> MQuery (DL.singleton (JsonPartialNumber (fromIntegral (length es))))
  _                     -> MQuery (DL.singleton (JsonPartialNumber 0))

limit :: Int -> MQuery a -> MQuery a
limit n (MQuery xs) = MQuery ((DL.fromList . take n . DL.toList) xs)

skip :: Int -> MQuery a -> MQuery a
skip n (MQuery xs) = MQuery ((DL.fromList . drop n . DL.toList) xs)

page :: Int -> Int -> MQuery a -> MQuery a
page size n (MQuery xs) = MQuery ((DL.fromList . take size . drop (size * n) . DL.toList) xs)

sorted :: Ord a => MQuery a -> MQuery a
sorted (MQuery xs) = MQuery ((DL.fromList . sort . DL.toList) xs)

onList :: Ord a => ([a] -> [a]) -> MQuery a -> MQuery a
onList f (MQuery xs) = MQuery ((DL.fromList . f . DL.toList) xs)

uniq :: Eq a => [a] -> [a]
uniq (a:b:cs) | a == b  =   uniq (b:cs)
uniq (a:b:cs)           = a:uniq (b:cs)
uniq cs                 = cs
