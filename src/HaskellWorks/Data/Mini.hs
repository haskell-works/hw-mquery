{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.Mini where

import qualified Data.DList                           as DL
import           HaskellWorks.Data.AtLeastSize
import           HaskellWorks.Data.Json.PartialValue
import           HaskellWorks.Data.Micro
import           Text.PrettyPrint.ANSI.Leijen

newtype Mini a = Mini a

newtype Row a = Row a

instance Pretty (Micro a) => Pretty (Mini [a]) where
  pretty (Mini xs) | xs `atLeastSize` 11  = text "[" <> nest 2 (prettyVs (take 10 (Micro `map` xs))) <> text ", ..]"
  pretty (Mini xs) | xs `atLeastSize` 1   = text "[" <> nest 2 (prettyVs (take 10 (Micro `map` xs))) <> text "]"
  pretty (Mini _ )                        = text "[]"

instance Pretty (Mini a) => Pretty (Mini (DL.DList a)) where
  pretty (Mini xs) = vcat (punctuate (text ",") ((pretty . Mini) `map` take 10 (DL.toList xs)))

instance Pretty (Mini a) => Pretty (Row (DL.DList a)) where
  pretty (Row xs) = vcat (((bold . yellow) (text "==> ") <>) `map` prettyRows)
    where
      prettyRows :: [Doc]
      prettyRows = (\row -> text (take 80 (displayS (renderCompact (pretty (Mini row))) []))) `map` take 10 (DL.toList xs)

instance Pretty (Mini JsonPartialValue) where
  pretty mjpv = case mjpv of
    Mini (JsonPartialString s   ) -> dullgreen  (text (show s))
    Mini (JsonPartialNumber n   ) -> cyan       (text (show n))
    Mini (JsonPartialObject []  ) -> text "{}"
    Mini (JsonPartialObject kvs ) -> case kvs of
      (_:_:_:_:_:_:_:_:_:_:_:_:_) -> text "{" <> prettyKvs kvs <> text ", ..}"
      []                          -> text "{}"
      _                           -> text "{" <> prettyKvs kvs <> text "}"
    Mini (JsonPartialArray []   ) -> text "[]"
    Mini (JsonPartialArray vs   ) | vs `atLeastSize` 11 -> text "[" <> nest 2 (prettyVs (Micro `map` take 10 vs)) <> text ", ..]"
    Mini (JsonPartialArray vs   ) | vs `atLeastSize` 1  -> text "[" <> nest 2 (prettyVs (Micro `map` take 10 vs)) <> text "]"
    Mini (JsonPartialArray _    )                       -> text "[]"
    Mini (JsonPartialBool w     ) -> red (text (show w))
    Mini  JsonPartialNull         -> text "null"
    Mini (JsonPartialError s    ) -> text "<error " <> text s <> text ">"

instance Pretty (Mini (String, JsonPartialValue)) where
  pretty (Mini (fieldName, jpv)) = text fieldName <> text ": " <> pretty (Mini jpv)
