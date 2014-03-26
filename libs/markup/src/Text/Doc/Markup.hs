{-# LANGUAGE NoMonomorphismRestriction #-}
module Text.Doc.Markup where

import           Data.ByteString.Lazy (ByteString)
import qualified Text.Doc.Parser      as Parser

parse :: String -> ByteString
parse code = parsed
    where parsed = Parser.parse code
