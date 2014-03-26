{-# LANGUAGE NoMonomorphismRestriction #-}

module Text.Doc.Markup where

--import           Data.ByteString.Lazy (ByteString)
import qualified Text.Doc.Parser      as Parser
import qualified Text.Parsec.Error    as Parsec

parse :: String -> Either Parsec.ParseError String -- ByteString
parse code = Parser.parse code
