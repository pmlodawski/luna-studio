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
import Data.List ( nub, sort )
import Data.Char ( isAlpha, toLower, toUpper, isSpace, digitToInt )
import qualified Text.Parsec.Expr as Expr
import Text.Parsec.Indent

import qualified Flowbox.Luna.Parser.Lexer   as L
import qualified Flowbox.Luna.Parser.AST.AST as AST

import qualified Text.Show.Pretty as PP
import System.TimeIt
import Debug.Trace


parens p        = between (L.symbol "(") (L.symbol ")") p

---------- Entities ----------

pIdent      = AST.Identifier   <$> L.identifier


----------------------

expr    = Expr.buildExpressionParser table term
      <?> "expression"

term    =  parens expr 
       <|> pIdent
       <|> pFuncExpr
       -- <|> L.natural
      <?> "simple expression"

table   = [ 
            --[prefix "-" negate, prefix "+" id ]
          --, [postfix "++" (+1)]
          --, [binary "*" (*) Expr.AssocLeft, binary "/" (div) Expr.AssocLeft ]
          --, [binary "+" (+) Expr.AssocLeft, binary "-" (-)   Expr.AssocLeft ]
            [binary "+"  (AST.Operator "+")  Expr.AssocLeft]
          , [binary ""   AST.callConstructor Expr.AssocLeft]
          , [binary "="  AST.Assignment      Expr.AssocLeft]
          ]
      
binary  name fun assoc = Expr.Infix   (L.reservedOp name *> return fun) assoc
prefix  name fun       = Expr.Prefix  (L.reservedOp name *> return fun)
postfix name fun       = Expr.Postfix (L.reservedOp name *> return fun)


pFunc         = AST.mkFunction <$  L.pDef 
                               <*> L.identifier 
                               <*  L.pBlockBegin
                               -- <*  L.eol  <* L.simpleSpace
                               -- <*  L.eol
                             -- <*> (pTuplePure (pOpExpr2 i) <<|> pTupleBody (pOpExpr2 i))
                             -- <*> pExprBlock i


--withBlock f a p = withPos $ do
--    r1 <- a
--    r2 <- option [] (indented >> block p)
--    return (f r1 r2)


--pFuncExpr     = withBlock (flip AST.setBody) pFunc (expr <* L.eol <* L.simpleSpace)

pFuncExpr = withPos $ do
    r1 <- pFunc
    r2 <- option [] $ iblock2 (expr)
    --r2 <- option [] (indented >> iblock expr)
    --r2 <- option [] (indented >> (block (expr <* L.eol <* L.simpleSpace)))
    return (AST.setBody r2 r1)


iblock2 p = do
    L.eol <* L.simpleSpace
    indented
    withPos $ do
        o1 <- p
        o2 <- myblock2 p
        return $ o1:o2

myblock2 p = do
    --L.eol <* L.simpleSpace
    r <- many (try(myblock_inner p))
    return r

myblock_inner p = do
    L.eol <* L.simpleSpace
    checkIndent
    p

--iblock p = do
--    L.eol <* L.simpleSpace
--    indented
--    out1 <- p
--    out2 <- many(try(myblock (L.eol *> L.simpleSpace) p))
--    return $ out1:out2


--myblock i p = do
--    i
--    withPos $ do
--    r <- checkIndent >> p
--    return r 


--exprs = concat <$> many (expr <* L.eol)

example = unlines [ "def f:"
                  , "    a"
                  , "    b"
                  , "    def g:"
                  , "        c"
                  , "    d"
				  ]

pProgram = expr


parse input = runIndent "Luna Parser" $ runParserT pProgram () "" input

main = do
    timeIt main_inner

main_inner = do
    --args <- getArgs
    --input <- if null args then return example else readFile $ head args
    putStrLn $ PP.ppShow $ forceEither $ parse example
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