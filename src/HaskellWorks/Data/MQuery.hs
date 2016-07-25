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
import           HaskellWorks.Data.Micro
import           HaskellWorks.Data.Row
import           Text.PrettyPrint.ANSI.Leijen

newtype MQuery a = MQuery (DL.DList a)

deriving instance Functor     MQuery
deriving instance Applicative MQuery
deriving instance Monad       MQuery
deriving instance Alternative MQuery
deriving instance MonadPlus   MQuery

instance Pretty (MQuery JsonPartialValue) where
  pretty (MQuery das) = pretty (Row das)

instance Pretty (MQuery String) where
  pretty (MQuery das) = case DL.toList das of
    as | as `atLeastSize` 100 -> text "[" <> prettyVs (take 100 as) <> text ", ..]"
    []                        -> text "[]"
    as                        -> text "[" <> prettyVs (take 100 as) <> text "]"

instance Pretty (MQuery (String, JsonPartialValue)) where
  pretty (MQuery das) = pretty (Row das)

hasKV :: String -> JsonPartialValue -> JsonPartialValue -> MQuery JsonPartialValue
hasKV k v (JsonPartialObject xs)  = if (k, v) `elem` xs then MQuery (DL.singleton (JsonPartialObject xs)) else MQuery DL.empty
hasKV _ _  _                      = MQuery DL.empty

inArray :: JsonPartialValue -> MQuery JsonPartialValue
inArray jpv = case jpv of
  JsonPartialArray es -> MQuery $ DL.fromList es
  _                   -> MQuery   DL.empty

dlTake :: Int -> DL.DList a -> DL.DList a
dlTake n = DL.fromList . take n . DL.toList

limit :: Int -> MQuery a -> MQuery a
limit n (MQuery xs) = MQuery ((DL.fromList . take n . DL.toList) xs)

skip :: Int -> MQuery a -> MQuery a
skip n (MQuery xs) = MQuery ((DL.fromList . drop n . DL.toList) xs)

page :: Int -> Int -> MQuery a -> MQuery a
page size n (MQuery xs) = MQuery ((DL.fromList . take size . drop (size * n) . DL.toList) xs)

inObject :: JsonPartialValue -> MQuery (String, JsonPartialValue)
inObject jpv = case jpv of
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

jsonSize :: JsonPartialValue -> MQuery JsonPartialValue
jsonSize jpv = case jpv of
  JsonPartialArray  es  -> MQuery (DL.singleton (JsonPartialNumber (fromIntegral (length es))))
  JsonPartialObject es  -> MQuery (DL.singleton (JsonPartialNumber (fromIntegral (length es))))
  _                     -> MQuery (DL.singleton (JsonPartialNumber 0))
