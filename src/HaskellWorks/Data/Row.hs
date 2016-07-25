{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}

module HaskellWorks.Data.Row where

import qualified Data.DList                   as DL
import           HaskellWorks.Data.Mini
import           Text.PrettyPrint.ANSI.Leijen

type MaxChars = Int

data Row a = Row MaxChars a

instance Pretty (Mini a) => Pretty (Row (DL.DList a)) where
  pretty (Row maxChars xs) = vcat (((bold . yellow) (text "==> ") <>) `map` prettyRows)
    where
      prettyRows :: [Doc]
      prettyRows = (\row -> text (take maxChars (displayS (renderCompact (pretty (Mini row))) []))) `map` DL.toList xs

prettyRowOfString :: Show a => Row (DL.DList a) -> Doc
prettyRowOfString (Row _ xs) = vcat (((bold . yellow) (text "==> ") <>) `map` prettyRows)
  where
    prettyRows :: [Doc]
    prettyRows = (text . show) `map` DL.toList xs
