{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.Mini where

import qualified Data.DList                           as DL
import           HaskellWorks.Data.AtLeastSize
import           HaskellWorks.Data.Json.PartialValue
import           HaskellWorks.Data.Micro
import           HaskellWorks.Data.Shows

newtype Mini a = Mini a

instance Show (Micro a) => Show (Mini [a]) where
  showsPrec _ (Mini xs) | xs `atLeastSize` 11  = ("[" ++) . showsVs (take 10 (Micro `map` xs)) . (", ..]"  ++)
  showsPrec _ (Mini xs) | xs `atLeastSize` 1   = ("[" ++) . showsVs (take 10 (Micro `map` xs)) . ("]"      ++)
  showsPrec _ (Mini _ )                        = ("[]"                                                     ++)

instance Show (Micro a) => Show (Mini (DL.DList a)) where
  showsPrec _ (Mini xs) = shows (Mini (DL.toList xs))

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
    Mini (JsonPartialArray vs   ) | vs `atLeastSize` 11 -> ("[" ++) . showsVs (Micro `map` take 10 vs) . (", ..]"  ++)
    Mini (JsonPartialArray vs   ) | vs `atLeastSize` 1  -> ("[" ++) . showsVs (Micro `map` take 10 vs) . ("]"      ++)
    Mini (JsonPartialArray _    )                       -> ("[]"                                                   ++)
    Mini (JsonPartialBool w     ) -> shows w
    Mini  JsonPartialNull         -> ("null" ++)
    Mini (JsonPartialError s    ) -> ("<error " ++) . shows s . (">" ++)

instance Show (Mini (String, JsonPartialValue)) where
  showsPrec _ (Mini (fieldName, jpv)) = shows fieldName . (": " ++) . shows (Mini jpv)
