{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.MQuery.Micro where

import Text.PrettyPrint.ANSI.Leijen

import qualified Data.DList as DL

newtype Micro a = Micro a

prettyVs :: Pretty a => [a] -> Doc
prettyVs (kv:kvs) = pretty kv <> foldl (<>) empty ((\jv -> text ", " <> pretty jv) `map` kvs)
prettyVs []       = empty

putPretty :: Pretty a => a -> IO ()
putPretty a = putDoc (pretty a <> hardline)

prettyKvs :: Pretty (Micro a) => [a] -> Doc
prettyKvs (kv:kvs) = pretty (Micro kv) <> foldl (<>) empty ((\jv -> text ", " <> pretty (Micro jv)) `map` kvs)
prettyKvs []       = empty

instance Pretty a => Pretty (Micro [a]) where
  pretty (Micro xs) = case length xs of
    xsLen | xsLen == 0  -> text "[]"
    xsLen | xsLen <= 10 -> text "[" <> prettyVs xs <> text "]"
    _                   -> text "[" <> prettyVs (take 10 xs) <> text ", ..]"

instance Pretty a => Pretty (Micro (DL.DList a)) where
  pretty (Micro dxs) = case DL.toList dxs of
    xs@(_:_:_:_:_:_:_:_:_:_:_:_:_) -> text "[" <> prettyVs (take 50 xs) <> text ", ..]"
    []                             -> text "[]"
    xs                             -> text "[" <> prettyVs          xs  <> text "]"
