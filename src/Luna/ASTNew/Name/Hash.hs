---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DeriveGeneric             #-}


module Luna.ASTNew.Name.Hash where

import Flowbox.Prelude
import Data.Char           (ord)


----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class Hashable a where
    hash :: a -> String
    

----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

hashReadableChar :: Char -> String
hashReadableChar = \case
    '!'  -> "bang"
    '@'  -> "at"
    '#'  -> "hash"
    '$'  -> "dollar"
    '%'  -> "percent"
    '^'  -> "caret"
    '&'  -> "ampersand"
    '*'  -> "star"
    '('  -> "lparen"
    ')'  -> "rparen"
    '_'  -> "underscore"
    '+'  -> "plus"
    '-'  -> "minus"
    '='  -> "equals"
    '~'  -> "tilde"
    '`'  -> "backtick"
    '{'  -> "lbrace"
    '}'  -> "rbrace"
    '['  -> "lbracket"
    ']'  -> "rbracket"
    ':'  -> "colon"
    ';'  -> "semicolon"
    '|'  -> "pipe"
    '/'  -> "slash"
    '\\' -> "backslash"
    '<'  -> "lt"
    '>'  -> "gt"
    ','  -> "comma"
    '.'  -> "dot"
    '?'  -> "qmark"
    '\'' -> "squote"
    '"'  -> "dquote"
    c    -> show $ ord c


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Hashable Char where
    hash c
       | (c >= 'a' && c <='z') || (c >= 'A' && c <='Z') = [c]
       | c == '_'                                       = "__"
       | otherwise                                      = "_" ++ hashReadableChar c

instance Hashable String where
    hash = concatMap hash