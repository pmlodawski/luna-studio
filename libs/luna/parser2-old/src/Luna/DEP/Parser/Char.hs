---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Luna.DEP.Parser.Char where

import           Control.Applicative
import           Flowbox.Prelude         hiding (lex)
import qualified Text.DEP.Parsers.Indent as Indent
import           Text.Parser.Char        hiding (spaces)
import           Text.Parser.Combinators


----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class Lexical a where
    lex :: CharParsing m => a -> m a


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

eolSeq :: CharParsing m => m String
eolSeq =   try (lex "\n\r")
       <|> try (lex "\r\n")
       <|> lex "\n"
       <|> lex "\r"
       <?> "end of line sequence"

eol = eolSeq *> return () <?> "end of line"


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Lexical Char where
    lex = char

instance Lexical String where
    lex = string
