---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Flowbox.Luna.Parser.Lexer where

import Control.Applicative
import Text.Parsec hiding (many, optional, (<|>))
import Data.List ( nub, sort )
import Data.Char ( digitToInt )

import Flowbox.Luna.Parser.Utils



identLetter  = alphaNum
identStart   = letter
commentLine  = "#"
commentStart = "#["
commentEnd   = "#]"


kDef = reserved "def"

pBlockBegin  = symbol  ':'
separator    = symbol  ','
parenL       = symbol  '('
parenR       = symbol  ')'
pTypeDecl    = symbols "::"

opStart      = oneOf "!#$%&*+./<=>?@\\^|-~"
opLetter     = opStart
reservedOpNames = ["=", "::", ":"]


pPath        = sepBy1 pIdent (symbol '.')


-----------------------------------------------------------
-- Keywords
-----------------------------------------------------------
reservedNames = ["def", "class", "interface", "from", "import", "as"]

-- entities
pDef        = reserved "def"
pClass      = reserved "class"
pInterface  = reserved "interface"

-- imports
pFrom       = reserved "from"
pImport     = reserved "import"
pAs         = reserved "as"


-----------------------------------------------------------
    -- Bracketing
-----------------------------------------------------------

parensed p        = between (parenL) (parenR) p


-----------------------------------------------------------
-- Chars & Strings
-----------------------------------------------------------
charLiteral     = lexeme (between (char '\'')
                                  (char '\'' <?> "end of character")
                                  characterChar )
                <?> "character"

characterChar   = charLetter <|> charEscape <?> "literal character"

charEscape      = char '\\' *> escapeCode

charLetter      = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

stringLiteral  = lexeme ( foldr (maybe id (:)) "" <$> between (char '"')
                                                      (char '"' <?> "end of string")
                                                      (many stringChar)
                                                  <?> "literal string" )

stringChar      =   Just <$> stringLetter
                <|> stringEscape
                <?> "string character"

stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

stringEscape    = char '\\' *> (    Nothing <$  escapeGap
	                            <|> Nothing <$  escapeEmpty
	                            <|> Just    <$> escapeCode
	                           )

escapeEmpty     = char '&'
escapeGap       = many1 space *> (char '\\' <?> "end of string gap")

-- escape codes
escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl <?> "escape code"

charControl     = (\code -> toEnum (fromEnum code - fromEnum 'A')) <$ char '^' <*> upper

charNum         = (toEnum.fromInteger) <$> (    decimal
	                                        <|> char 'o' *> number 8 octDigit
	                                        <|> char 'x' *> number 16 hexDigit
	                                       )


charEsc         = choice (map parseEsc escMap) where
                      parseEsc (c,code) = char c *> return code

charAscii       = choice (map parseAscii asciiMap) where
                      parseAscii (asc,code) = try (string asc *> return code)


-- escape code tables
escMap          = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                   "FS","GS","RS","US","SP"]
ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                   "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                   "CAN","SUB","ESC","DEL"]

ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                   '\EM','\FS','\GS','\RS','\US','\SP']
ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                   '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                   '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']


-----------------------------------------------------------
-- Numbers
-----------------------------------------------------------
naturalOrFloat  = lexeme (natFloat) <?> "number"

float           = lexeme floating   <?> "float"
integer         = lexeme int        <?> "integer"
natural         = lexeme nat        <?> "natural"

integerStr      = lexeme intStr     <?> "integer"

-- floats
floating        = fractExponent <$*> decimal

natFloat        =   char '0' *> zeroNumFloat
                <|> decimalFloat


zeroNumFloat    =   (\n -> return $ Left n) <$*> (hexadecimal <|> octal)
                <|> decimalFloat
                <|> fractFloat 0
                <|> return (Left 0)

decimalFloat    = (\n -> option (Left n) (fractFloat n)) <$*> decimal

fractFloat n    = Right <$> fractExponent n

fractExponent n =   (\fract expo -> (fromInteger n + fract)*expo) <$> fraction <*> option 1.0 exponent'
                <|> (\      expo -> (fromInteger n)*expo)         <$> exponent'


fraction        = (foldr op 0.0) <$ char '.' <*> (many1 digit <?> "fraction") <?> "fraction" where
	op d f      = (f + fromIntegral (digitToInt d))/10.0


exponent'       = (\f e -> power (f e)) <$ oneOf "eE" <*> sign <*> (decimal <?> "exponent") <?> "exponent" where
	power e  | e < 0      = 1.0/power(-e)
             | otherwise  = fromInteger (10^e)


-- integers and naturals
intStr          = ((:) <$> lexeme signStr <*> natStr) <|> natStr

signStr         =   char '-'
                <|> char '+'

natStr          = zeroNumberStr <|> decimalStr

zeroNumberStr   = char '0' *> (hexadecimalStr <|> octalStr <|> decimalStr <|> return "") <?> ""

decimalStr      = numberStr digit
hexadecimalStr  = oneOf "xX" *> numberStr hexDigit
octalStr        = oneOf "oO" *> numberStr octDigit

numberStr baseDigit = many1 baseDigit

---

int             = ($) <$> lexeme sign <*> nat

sign            =   negate <$ char '-'
                <|> id     <$ char '+'
                <|> return id

nat             = zeroNumber <|> decimal

zeroNumber      = char '0' *> (hexadecimal <|> octal <|> decimal <|> return 0) <?> ""

decimal         = number 10 digit
hexadecimal     = oneOf "xX" *> number 16 hexDigit
octal           = oneOf "oO" *> number 8 octDigit

number base baseDigit = do
        digits <- numberStr baseDigit
        let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        seq n (return n)
        

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

pIdentVar     = pIdentLower
pIdentType    = pIdentUpper
pIdentTypeVar = pIdentLower

pIdent        = pIdentLower <|> pIdentUpper <?> "identifier"

pIdentUpper   = mkIdent ((:) <$> upper <*> many identLetter) <?> "uppercase identifier"

pIdentLower   = mkIdent ((:) <$> lower <*> many identLetter) <?> "lowercase identifier"

mkIdent p     = lexeme $ try $ checkIf isReservedName "reserved word " p

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
lexeme p    = p <* skipMany pSpaces1

symbols name = try $ lexeme (string name)
symbol  name = lexeme (char name)

pSpace      = satisfy (`elem` "\t\f\v ") 
pSpacesBase = many1 pSpace <|> try(multiLineComment) <|> oneLineComment <?> ""
pSpaces1    = many1 pSpacesBase
pSpaces     = many  pSpacesBase


oneLineComment   = try (string commentLine) *> many (satisfy (/= '\n'))

multiLineComment = try (string commentStart) *> inComment

inComment =   try (string commentEnd)            *> return ""
          <|> ((++) <$> multiLineComment        <*> inComment)
          <|> ((++) <$> many1 (noneOf startEnd) <*> inComment)
          <|> oneOf startEnd                     *> inComment
          <?> "end of comment"
          where
          	startEnd   = nub (commentEnd ++ commentStart)


eol = (char '\n' <|> (char '\r' >> option '\n' (char '\n'))) >> return ()


