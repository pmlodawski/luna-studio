---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DeriveGeneric             #-}


module Luna.Syntax.Name.Hash where

import           Data.Char      (ord)
import qualified Data.Text.Lazy as Text

import Flowbox.Prelude

----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class Hashable a b where
    hash :: a -> b
    

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
    c    -> fromString . show $ ord c


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Hashable Char String where
    hash c
       | (c >= 'a' && c <='z') || (c >= 'A' && c <='Z') = [c]
       | (c >= '0' && c <='9')                          = [c]
       | c == '_'                                       = "__"
       | c == '#'                                       = "_" -- FIXME [wd]: just a dirty fix for hast gen
       | otherwise                                      = "_" ++ hashReadableChar c


instance Hashable Text Text where
    hash t = if c == '@' then fromString (hashToUnderscore cc) -- FIXME [wd]: just a dirty fix for hast gen
                         else Text.fromChunks . fmap (fromString . hash) $ Text.unpack t
        where str@(c:cc) = Text.unpack t
        

instance Hashable String String where
    hash ('@':t) = hashToUnderscore t
    hash  val    = concat $ fmap hash val


hashToUnderscore = \case
    [] -> []
    (s:ss) -> s' : hashToUnderscore ss where
        s' = if s == '#' then '_'
                         else s
