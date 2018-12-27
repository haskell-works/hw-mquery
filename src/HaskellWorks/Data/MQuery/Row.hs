{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.MQuery.Row where

import Text.PrettyPrint.ANSI.Leijen

import qualified Data.DList as DL

type MaxChars = Int

data Row a = Row MaxChars a

instance Pretty a => Pretty (Row (DL.DList a)) where
  pretty (Row maxChars xs) = vcat (((bold . yellow) (text "==> ") <>) `map` prettyRows)
    where prettyRows :: [Doc]
          prettyRows = (\row -> text (take maxChars (displayS (renderCompact (pretty row)) []))) `map` DL.toList xs

prettyRowOfString :: Show a => Row (DL.DList a) -> Doc
prettyRowOfString (Row _ xs) = vcat (((bold . yellow) (text "==> ") <>) `map` prettyRows)
  where prettyRows :: [Doc]
        prettyRows = (text . show) `map` DL.toList xs
