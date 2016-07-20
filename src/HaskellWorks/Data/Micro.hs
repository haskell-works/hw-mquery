{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.Micro where

import qualified Data.DList                           as DL
import           Data.List
import           HaskellWorks.Data.Json.PartialValue

newtype Micro a = Micro a

instance Show (Micro JsonPartialValue) where
  showsPrec _ v = case v of
    Micro (JsonPartialString s ) -> shows s
    Micro (JsonPartialNumber n ) -> shows n
    Micro (JsonPartialObject []) -> ("{}" ++)
    Micro (JsonPartialObject _ ) -> ("{..}" ++)
    Micro (JsonPartialArray [] ) -> ("[]" ++)
    Micro (JsonPartialArray _  ) -> ("[..]" ++)
    Micro (JsonPartialBool w   ) -> shows w
    Micro  JsonPartialNull       -> ("null" ++)
    Micro (JsonPartialError s  ) -> ("<error " ++) . shows s . (">" ++)

instance Show (Micro (String, JsonPartialValue)) where
  showsPrec _ (Micro (fieldName, jpv)) = shows fieldName . (": " ++) . shows (Micro jpv)

instance Show a => Show (Micro [a]) where
  show (Micro xs) = case length xs of
    xsLen | xsLen == 0    -> "[]"
    xsLen | xsLen <= 50   -> "[" ++ intercalate ", " (show `map` xs) ++ "]"
    _                     -> "[" ++ intercalate ", " (show `map` take 50 xs) ++ ", ..]"

instance Show a => Show (Micro (DL.DList a)) where
  showsPrec _ (Micro dxs) = case DL.toList dxs of
    xs@(_:_:_:_:_:_:_:_:_:_:_:_:_)  -> (("[" ++ intercalate ", " (show `map` take 50 xs) ++ ", ..]") ++)
    []                              -> ("[]" ++)
    xs                              -> (("[" ++ intercalate ", " (show `map` xs) ++ "]") ++)
