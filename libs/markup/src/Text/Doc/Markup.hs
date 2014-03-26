{-# LANGUAGE NoMonomorphismRestriction #-}
module Text.Doc.Markup where

import           Data.ByteString (ByteString)
import qualified Text.Doc.Parser      as Parser
-- import qualified Text.Parsec.Error    as Parsec

-- parse :: String -> Either Parsec.ParseError ByteString
--parse :: String -> ByteString
parse code = parsed
    where parsed = Parser.parse code
