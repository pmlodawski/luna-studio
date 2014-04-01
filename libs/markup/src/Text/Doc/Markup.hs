{-# LANGUAGE NoMonomorphismRestriction #-}
module Text.Doc.Markup where

--import           Data.ByteString.Lazy (ByteString)
import qualified Text.Doc.Parser      as Parser
--import           Text.Parsec.Error    (ParseError)

--parse :: String -> Either ParseError ByteString
parse code = parsed
    where parsed = Parser.parse code

parse_test code = parsed
    where parsed = Parser.parse_test code
