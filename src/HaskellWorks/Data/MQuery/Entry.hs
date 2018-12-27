module HaskellWorks.Data.MQuery.Entry where

import Text.PrettyPrint.ANSI.Leijen

data Entry k v = Entry k v

instance (Pretty k, Pretty v) => Pretty (Entry k v) where
  pretty (Entry k v) = pretty k <> text ": " <> pretty v
