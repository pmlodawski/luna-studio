---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Flowbox.Luna.Parser.Parser where

import Control.Applicative
import Data.Char (isSpace)
import Data.Either.Utils (forceEither)
import Data.Monoid
import System.Environment (getArgs)
import Text.Parsec hiding (parse, many, optional, (<|>))
import Text.Parsec.Indent
import Data.List ( nub, sort )
import Data.Char ( isAlpha, toLower, toUpper, isSpace, digitToInt )
import qualified Text.Parsec.Expr as Expr

import qualified Flowbox.Luna.Parser.Lexer   as L
import qualified Flowbox.Luna.Parser.AST.AST as AST


parens p        = between (L.symbol "(") (L.symbol ")") p

----------------------

expr    = Expr.buildExpressionParser table term
      <?> "expression"

term    =  parens expr 
      <|> L.natural
      <?> "simple expression"

table   = [ [prefix "-" negate, prefix "+" id ]
          , [postfix "++" (+1)]
          , [binary "*" (*) Expr.AssocLeft, binary "/" (div) Expr.AssocLeft ]
          , [binary "+" (+) Expr.AssocLeft, binary "-" (-)   Expr.AssocLeft ]
          , [binary ""  (+) Expr.AssocLeft, binary "-" (-)   Expr.AssocLeft ]
          ]
      
binary  name fun assoc = Expr.Infix (do{ L.reservedOp name; return fun }) assoc
prefix  name fun       = Expr.Prefix (do{ L.reservedOp name; return fun })
postfix name fun       = Expr.Postfix (do{ L.reservedOp name; return fun })


pFunc         = AST.Function <$  L.pDef 
                             <*> L.identifier 
                             -- <*> (pTuplePure (pOpExpr2 i) <<|> pTupleBody (pOpExpr2 i))
                             -- <*> pExprBlock i


example = unlines [ "12  1"
				  ]

pProgram = expr


parse input = runIndent "Luna Parser" $ runParserT pProgram () "" input

main = do
    --args <- getArgs
    --input <- if null args then return example else readFile $ head args
    print $ parse example
    return ()
    --putStrLn $ serializeIndentedTree $ forceEither $ parseIndentedTree input




--parse input = runIndent "" $ runParserT aTree () "" input



--data Tree = Node [Tree] | Leaf String deriving (Show)

--serializeIndentedTree tree = drop 2 $ s (-1) tree
--  where
--    s i (Node children) = "\n" <> (concat $ replicate i "    ") <> (concat $ map (s (i+1)) children)
--    s _ (Leaf text)     = text <> " "

--main = do
--    args <- getArgs
--    input <- if null args then return example else readFile $ head args
--    print $ parse input



--aTree = Node <$> many aNode

--aNode = spaces *> withBlock makeNode aNodeHeader aNode

--aNodeHeader = many1 aLeaf <* spaces

--aLeaf = Leaf <$> (many1 (satisfy (not . isSpace)) <* many (oneOf " \t"))

--makeNode leaves nodes = Node $ leaves <> nodes

--example = unlines [
--    "lorem ipsum",
--    "    dolor",
--    "    sit amet",
--    "    consectetur",
--    "        adipiscing elit dapibus",
--    "    sodales",
--    "urna",
--    "    facilisis"
--  ]