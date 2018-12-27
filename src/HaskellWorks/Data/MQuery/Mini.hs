{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.MQuery.Mini where

import HaskellWorks.Data.MQuery.AtLeastSize
import HaskellWorks.Data.MQuery.Micro
import Text.PrettyPrint.ANSI.Leijen

import qualified Data.DList as DL

newtype Mini a = Mini a

instance Pretty (Micro a) => Pretty (Mini [a]) where
  pretty (Mini xs) | xs `atLeastSize` 11  = text "[" <> nest 2 (prettyVs (take 10 (Micro `map` xs))) <> text ", ..]"
  pretty (Mini xs) | xs `atLeastSize` 1   = text "[" <> nest 2 (prettyVs (take 10 (Micro `map` xs))) <> text "]"
  pretty (Mini _ ) = text "[]"

instance Pretty (Mini a) => Pretty (Mini (DL.DList a)) where
  pretty (Mini xs) = vcat (punctuate (text ",") ((pretty . Mini) `map` take 10 (DL.toList xs)))
