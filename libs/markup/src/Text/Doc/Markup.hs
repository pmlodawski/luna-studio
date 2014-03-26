{-# LANGUAGE NoMonomorphismRestriction #-}
module Text.Doc.Markup where

--<<<<<<< HEAD
--import           Data.ByteString (ByteString)
--=======
--import           Data.ByteString.Lazy (ByteString)
-->>>>>>> f406e1359a1886e2f5f5d2c3d2f2ef86400802de
import qualified Text.Doc.Parser      as Parser
import qualified Text.Parsec.Error    as Parsec

--<<<<<<< HEAD
-- parse :: String -> Either Parsec.ParseError ByteString
--parse :: String -> ByteString
--parse code = parsed
    --where parsed = Parser.parse code
--=======
parse :: String -> Either Parsec.ParseError String -- ByteString
parse code = Parser.parse code
-->>>>>>> f406e1359a1886e2f5f5d2c3d2f2ef86400802de
