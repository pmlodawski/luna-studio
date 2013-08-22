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
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.Indent
import Data.List ( nub, sort )
import Data.Char ( isAlpha, toLower, toUpper, isSpace, digitToInt )

import qualified Flowbox.Luna.Parser.Lexer   as L
import qualified Flowbox.Luna.Parser.AST.AST as AST


--------------------

pFunc         = AST.Function <$  L.pDef 
                             <*> L.identifier 
                             -- <*> (pTuplePure (pOpExpr2 i) <<|> pTupleBody (pOpExpr2 i))
                             -- <*> pExprBlock i


example = unlines [ "def f"
				  ]

test = pFunc


main = do
    --args <- getArgs
    --input <- if null args then return example else readFile $ head args
    print $ parse (test) "(unknown)" example
    return ()
    --putStrLn $ serializeIndentedTree $ forceEither $ parseIndentedTree input