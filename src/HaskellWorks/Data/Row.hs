{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}

module HaskellWorks.Data.Row where

import qualified Data.DList                   as DL
import           HaskellWorks.Data.Mini
import           Text.PrettyPrint.ANSI.Leijen

newtype Row a = Row a

instance Pretty (Mini a) => Pretty (Row (DL.DList a)) where
  pretty (Row xs) = vcat (((bold . yellow) (text "==> ") <>) `map` prettyRows)
    where
      prettyRows :: [Doc]
      prettyRows = (\row -> text (take 120 (displayS (renderCompact (pretty (Mini row))) []))) `map` DL.toList xs
