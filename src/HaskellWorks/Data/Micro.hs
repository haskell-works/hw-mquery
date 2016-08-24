{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HaskellWorks.Data.Micro where

import qualified Data.DList                           as DL
-- import           HaskellWorks.Data.Json.PartialValue
import           Text.PrettyPrint.ANSI.Leijen

newtype Micro a = Micro a

prettyVs :: Pretty a => [a] -> Doc
prettyVs (kv:kvs) = pretty kv <> foldl (<>) empty ((\jv -> text ", " <> pretty jv) `map` kvs)
prettyVs []       = empty

putPretty :: Pretty a => a -> IO ()
putPretty a = putDoc (pretty a <> hardline)

prettyKvs :: Pretty (Micro a) => [a] -> Doc
prettyKvs (kv:kvs) = pretty (Micro kv) <> foldl (<>) empty ((\jv -> text ", " <> pretty (Micro jv)) `map` kvs)
prettyKvs []       = empty

-- instance Pretty (Micro JsonPartialValue) where
--   pretty (Micro (JsonPartialString s )) = dullgreen (text (show s))
--   pretty (Micro (JsonPartialNumber n )) = cyan      (text (show n))
--   pretty (Micro (JsonPartialObject [])) = text "{}"
--   pretty (Micro (JsonPartialObject _ )) = text "{..}"
--   pretty (Micro (JsonPartialArray [] )) = text "[]"
--   pretty (Micro (JsonPartialArray _  )) = text "[..]"
--   pretty (Micro (JsonPartialBool w   )) = red (text (show w))
--   pretty (Micro  JsonPartialNull      ) = text "null"
--   pretty (Micro (JsonPartialError s  )) = text "<error " <> text s <> text ">"
--
-- instance Pretty (Micro (String, JsonPartialValue)) where
--   pretty (Micro (fieldName, jpv)) = red (text (show fieldName)) <> text ": " <> pretty (Micro jpv)

instance Pretty a => Pretty (Micro [a]) where
  pretty (Micro xs) = case length xs of
    xsLen | xsLen == 0    -> text "[]"
    xsLen | xsLen <= 10   -> text "[" <> prettyVs xs <> text "]"
    _                     -> text "[" <> prettyVs (take 10 xs) <> text ", ..]"

instance Pretty a => Pretty (Micro (DL.DList a)) where
  pretty (Micro dxs) = case DL.toList dxs of
    xs@(_:_:_:_:_:_:_:_:_:_:_:_:_)  -> text "[" <> prettyVs (take 50 xs) <> text ", ..]"
    []                              -> text "[]"
    xs                              -> text "[" <> prettyVs          xs  <> text "]"
