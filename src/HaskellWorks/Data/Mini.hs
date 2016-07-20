{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.Mini where

import qualified Data.DList                           as DL
import           Data.List
import           HaskellWorks.Data.Json.PartialValue
import           HaskellWorks.Data.Micro

newtype Mini a = Mini a

instance Show a => Show (Mini [a]) where
  show (Mini xs) = case length xs of
    xsLen | xsLen == 0    -> "[]"
    xsLen | xsLen <= 50   -> "[" ++ intercalate ", " (show `map` xs) ++ "]"
    _                     -> "[" ++ intercalate ", " (show `map` take 50 xs) ++ ", ..]"

instance Show a => Show (Mini (DL.DList a)) where
  showsPrec _ (Mini dxs) = case DL.toList dxs of
    xs@(_:_:_:_:_:_:_:_:_:_:_:_:_)  -> (("[" ++ intercalate ", " (show `map` take 50 xs) ++ ", ..]") ++)
    []                              -> ("[]" ++)
    xs                              -> (("[" ++ intercalate ", " (show `map` xs) ++ "]") ++)

instance Show (Mini JsonPartialValue) where
  showsPrec _ mjpv = case mjpv of
    Mini (JsonPartialString s   ) -> shows s
    Mini (JsonPartialNumber n   ) -> shows n
    Mini (JsonPartialObject []  ) -> ("{}" ++)
    Mini (JsonPartialObject kvs ) -> case kvs of
      (_:_:_:_:_:_:_:_:_:_:_:_:_) -> ("{" ++) . showKvs kvs . (", ..}" ++)
      []                          -> ("{}" ++)
      _                           -> ("{" ++) . showKvs kvs . ("}" ++)
    Mini (JsonPartialArray []   ) -> ("[]" ++)
    Mini (JsonPartialArray vs   ) -> case vs of
      (_:_:_:_:_:_:_:_:_:_:_:_:_) -> ("[" ++) . showVs vs . (", ..]" ++)
      []                          -> ("[]" ++)
      _                           -> ("[" ++) . showVs vs . ("]" ++)
    Mini (JsonPartialBool w     ) -> shows w
    Mini  JsonPartialNull         -> ("null" ++)
    Mini (JsonPartialError s    ) -> ("<error " ++) . shows s . (">" ++)
    where showKvs :: [(String, JsonPartialValue)] -> String -> String
          showKvs (kv:kvs) = shows (Micro kv) . foldl (.) id ((\jv -> (", " ++) . shows (Micro jv)) `map` kvs)
          showKvs []       = id
          showVs :: [JsonPartialValue] -> String -> String
          showVs (kv:kvs) = shows (Micro kv) . foldl (.) id ((\jv -> (", " ++) . shows (Micro jv)) `map` kvs)
          showVs []       = id

instance Show (Mini (String, JsonPartialValue)) where
  showsPrec _ (Mini (fieldName, jpv)) = shows fieldName . (": " ++) . shows (Mini jpv)
