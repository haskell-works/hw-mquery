{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module HaskellWorks.Data.MQuery where

import           Control.Monad
import           Data.List
import qualified Data.DList                           as DL
import           GHC.Base
import           HaskellWorks.Data.Entry
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

class IsPredicate a where
  type ArgOf a
  toPredicate :: ArgOf a -> (a -> Bool)

mQuery :: MQuery a -> DL.DList a
mQuery (MQuery a) = a

instance ToBool (MQuery a) where
  toBool = toBool . mQuery

instance Pretty (MQuery JsonPartialValue) where
  pretty = pretty . Row 120 . mQuery

instance Pretty (MQuery String) where
  pretty x = prettyRowOfString (Row 120 (mQuery x))

instance Pretty (MQuery Integer) where
  pretty x = prettyRowOfString (Row 120 (mQuery x))

instance Pretty (MQuery Int) where
  pretty x = prettyRowOfString (Row 120 (mQuery x))

instance Pretty (MQuery (Entry String JsonPartialValue)) where
  pretty (MQuery das) = pretty (Row 120 das)

hasKV :: String -> JsonPartialValue -> JsonPartialValue -> MQuery JsonPartialValue
hasKV k v (JsonPartialObject xs)  = if (k, v) `elem` xs then MQuery (DL.singleton (JsonPartialObject xs)) else MQuery DL.empty
hasKV _ _  _                      = MQuery DL.empty

item :: JsonPartialValue -> MQuery JsonPartialValue
item jpv = case jpv of
  JsonPartialArray es -> MQuery $ DL.fromList es
  _                   -> MQuery   DL.empty

entry :: JsonPartialValue -> MQuery (Entry String JsonPartialValue)
entry jpv = case jpv of
  JsonPartialObject fs  -> MQuery $ DL.fromList (uncurry Entry `map` fs)
  _                     -> MQuery   DL.empty

asString :: JsonPartialValue -> MQuery String
asString jpv = case jpv of
  JsonPartialString s -> MQuery $ DL.singleton s
  _                   -> MQuery   DL.empty

asInteger :: JsonPartialValue -> MQuery Integer
asInteger jpv = case jpv of
  JsonPartialNumber n -> MQuery $ DL.singleton (floor n)
  _                   -> MQuery   DL.empty

castAsInteger :: JsonPartialValue -> MQuery Integer
castAsInteger jpv = case jpv of
  JsonPartialString n -> MQuery $ DL.singleton (read n)
  JsonPartialNumber n -> MQuery $ DL.singleton (floor n)
  _                   -> MQuery   DL.empty

named :: String -> Entry String JsonPartialValue -> MQuery JsonPartialValue
named fieldName (Entry fieldName' jpv) | fieldName == fieldName'  = MQuery $ DL.singleton jpv
named _         _                                                 = MQuery   DL.empty

satisfying :: (a -> Bool) -> a -> MQuery a
satisfying p a | p a  = MQuery $ DL.singleton a
satisfying _ _        = MQuery   DL.empty

key :: Entry k v -> MQuery k
key (Entry k _) = MQuery $ DL.singleton k

value :: Entry k v -> MQuery v
value (Entry _ v) = MQuery $ DL.singleton v

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

having :: (a -> MQuery b) -> a -> MQuery a
having p a = case p a of
  MQuery das -> case DL.toList das of
    _:_ -> MQuery (DL.singleton a)
    _   -> MQuery  DL.empty

valueOf :: Eq a => a -> a -> MQuery a
valueOf a b = if a == b then MQuery (DL.singleton b) else MQuery DL.empty

limit :: Int -> MQuery a -> MQuery a
limit n (MQuery xs) = MQuery ((DL.fromList . take n . DL.toList) xs)

skip :: Int -> MQuery a -> MQuery a
skip n (MQuery xs) = MQuery ((DL.fromList . drop n . DL.toList) xs)

page :: Int -> Int -> MQuery a -> MQuery a
page size n (MQuery xs) = MQuery ((DL.fromList . take size . drop (size * n) . DL.toList) xs)

sorted :: Ord a => MQuery a -> MQuery a
sorted (MQuery xs) = MQuery ((DL.fromList . sort . DL.toList) xs)

onList :: ([a] -> [a]) -> MQuery a -> MQuery a
onList f (MQuery xs) = MQuery ((DL.fromList . f . DL.toList) xs)

aggregate :: ([a] -> b) -> MQuery a -> MQuery b
aggregate f (MQuery xs) = MQuery (DL.fromList [f (DL.toList xs)])

uniq :: Eq a => [a] -> [a]
uniq (a:b:cs) | a == b  =   uniq (b:cs)
uniq (a:b:cs)           = a:uniq (b:cs)
uniq cs                 = cs
