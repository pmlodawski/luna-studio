---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Text.Doc.Lexer where

import Control.Applicative
import Text.Parsec         hiding (many, optional, parse, (<|>))



pH1 = string "==="
pH2 = string "=="
pH3 = string "="
headingTxt = many $ noneOf "="

pTextBoldItalic = string "***"
pTextItalic         = string "**"
pTextBold                 = string "*"
formattedText = many $ noneOf "*\n\r"

pCodeLangBegin          = string $ "{{"
pCodeLang              = many $ noneOf "}"
pCodeLangEnd            = string "}}"

pBlockBegin     = eol
pCodeLineBegin         = string $ replicate 4 ' '
pCodeInline         = string "`"
inlineCode = many $ noneOf "`"

pQuoteLineBegin = string "> "

pHR = string "---"

pUlLiBegin = string "- "
pOlLiBegin = string "# "

pLinkBegin                   = string "http://"
pLinkContentBegin = string "["
pLinkContentEnd   = string "]"
pLinkContent      = many $ noneOf "]"
pLinkAddrBegin          = string "("
pLinkAddrEnd          = string ")"
pLinkAddr                    = many $ noneOf ")"
pImageBegin           = string "!"

pNotWhitespace = noneOf " \t\n\r\f\v" -- wouldn't \s work? mabe - gotta check later on

pSpace      = satisfy (`elem` "\t\f\v ") <?> ""
pSpacesBase = many1 pSpace <?> ""
pSpaces     = many  pSpacesBase <?> ""

eol = (char '\n' <|> (char '\r' >> option '\n' (char '\n'))) >> return () <?> ""
