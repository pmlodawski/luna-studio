---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Luna.Parser.Char where

import           Flowbox.Prelude         hiding (lex)
import           Control.Applicative
import           Text.Parser.Combinators
import           Text.Parser.Char        hiding (spaces)
import qualified Text.Parsers.Indent     as Indent


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