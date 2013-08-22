---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Flowbox.Luna.Parser.Parser where

--import Debug.Trace
--import System.TimeIt


---- file: ch16/csv6.hs
--import Text.ParserCombinators.Parsec

--csvFile = endBy line eol
--line = sepBy cell (char ',')
--cell = many (noneOf ",\n\r")

--eol =   try (string "\n\r")
--    <|> try (string "\r\n")
--    <|> string "\n"
--    <|> string "\r"
--    <?> "end of line"

--parseCSV :: String -> Either ParseError [[String]]
--parseCSV input = parse csvFile "(unknown)" input


--main :: IO ()
--main = do
--    print $ parseCSV "line1\r\nline2\nline3\n\rline4\rline5\n"


--import Text.Parsec hiding (State)
--import qualified Text.Parsec.Indent as IP
--import Control.Monad.State

--type IParser a = ParsecT String () (State SourcePos) a

--iParse :: IParser a -> SourceName -> String -> Either ParseError a
--iParse aParser source_name input =
--  IP.runIndent source_name $ runParserT aParser () source_name input

--input_text :: String
--input_text = unlines [
--    "listName",
--    "  item1",
--    "  item2",
--    "  item3"
--  ]

--main :: IO ()
--main = do
--  print $ iParse aNamedList "indented_example" input_text

--data NamedList = NamedList Name [Item]
--  deriving (Show)

--type Name = String
--type Item = String

--aNamedList :: IParser NamedList
--aNamedList = do
--  b <- IP.withBlock NamedList aName anItem
--  spaces
--  return b

--aName :: IParser Name
--aName = do
--  s <- many1 alphaNum
--  _ <- char ':'
--  spaces
--  return s

--anItem :: IParser Item
--anItem = do
--  i <- many1 alphaNum
--  spaces
--  return i



import Control.Applicative
import Data.Char (isSpace)
import Data.Either.Utils (forceEither)
import Data.Monoid
import System.Environment (getArgs)
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.Indent
import Data.List ( nub, sort )

data Tree = Node [Tree] | Leaf String deriving(Show)

serializeIndentedTree tree = drop 2 $ s (-1) tree
  where
    s i (Node children) = "\n" <> (concat $ replicate i "    ") <> (concat $ map (s (i+1)) children)
    s _ (Leaf text)     = text <> " "



parseIndentedTree input = runIndent "" $ runParserT aTree () "" input

aTree = Node <$> many aNode

aNode = spaces *> withBlock makeNode aNodeHeader aNode

aNodeHeader = many1 aLeaf <* spaces

aLeaf = Leaf <$> (many1 (satisfy (not . isSpace)) <* many (oneOf " \t"))

makeNode leaves nodes = Node $ leaves <> nodes



identLetter  = alphaNum
identStart   = letter
commentLine  = "#"
commentStart = "#["
commentEnd   = "#]"

reservedNames = ["def", "class", "interface"]

kDef = reserved "def"

opStart      = oneOf ":!#$%&*+./<=>?@\\^|-~"
opLetter     = opStart
reservedOpNames = ["="]

-----------------------------------------------------------
-- Operators & reserved ops
-----------------------------------------------------------
reservedOp name = lexeme $ try $ string name <* (notFollowedBy opLetter <?> ("end of " ++ show name))

operator = lexeme $ try $ checkIf isReservedOp "reserved operator " oper

oper = (:) <$> opStart <*> many opLetter <?> "operator"

isReservedOp name = isReserved (sort reservedOpNames) name

-----------------------------------------------------------
-- Identifiers & Reserved words
-----------------------------------------------------------
reserved name = lexeme $ try $ string name <* (notFollowedBy identLetter <?> ("end of " ++ show name))

identifier = lexeme $ try $ checkIf isReservedName "reserved word " ident

ident = (:) <$> identStart <*> many identLetter <?> "identifier"

isReserved names name
    = scan names
    where
      scan []       = False
      scan (r:rs)   = case (compare r name) of
                        LT  -> scan rs
                        EQ  -> True
                        GT  -> False

isReservedName name = isReserved (sort reservedNames) name


-----------------------------------------------------------
-- White space & symbols
-----------------------------------------------------------
lexeme p    = p <* whiteSpace

whiteSpace  = skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
simpleSpace = skipMany1 (satisfy isSpace)

oneLineComment =
        do{ try (string commentLine)
          ; skipMany (satisfy (/= '\n'))
          ; return ()
          }

multiLineComment =
        do { try (string commentStart)
           ; inComment
           }

inComment
        =   do{ try (string commentEnd) ; return () }
        <|> do{ multiLineComment                     ; inComment }
        <|> do{ skipMany1 (noneOf startEnd)          ; inComment }
        <|> do{ oneOf startEnd                       ; inComment }
        <?> "end of comment"
        where
          startEnd   = nub (commentEnd ++ commentStart)

-----------------------------------------------------------
-- Utils
-----------------------------------------------------------

checkIf f msg p = do
	obj <- p
	if (f obj)
		then unexpected (msg ++ show obj)
		else return obj

-----------------------------------------------------------


example = unlines [
    "def+="
  ]


pSym s         = char s
pSyms []       = return []
pSyms (x : xs) = (:) <$> pSym x <*> pSyms xs

test = try(pSyms "aa") <|> pSyms "ab"


main = do
    --args <- getArgs
    --input <- if null args then return example else readFile $ head args
    print $ parse ((\x y -> [x,y]) <$> kDef <*> operator) "(unknown)" example
    return ()
    --putStrLn $ serializeIndentedTree $ forceEither $ parseIndentedTree input