{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Lexer where

import Control.Applicative
import Control.Monad (MonadPlus(..), when)
import Data.Char
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.List (foldl')
import Data.Monoid
import Data.String
import Data.Text hiding (empty,zip,foldl,foldl')
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token.Highlight
import Text.Parser.Token
import Flowbox.Prelude as Prelude hiding (op)
import qualified Luna.AST.Lit.Number as Number



reservedIdents :: [String]
reservedIdents = ["alias", "as", "case", "class", "def", "else", "from", "if", "interface", "import", "in", "type"]


opToken p = token (highlight Operator p)

opLetter = oneOf "!#$%&*+./<=>?\\^|-~"
opStart = opLetter

operator = opToken ((:) <$> opStart <*> many opLetter <?> "operator")

identStyle = IdentifierStyle
  { _styleName      = "identifier"
  , _styleStart     = letter <|> char '_'
  , _styleLetter    = alphaNum <|> oneOf "_'"
  , _styleReserved  = HashSet.fromList reservedIdents
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

varStyle :: TokenParsing m => IdentifierStyle m
varStyle = identStyle { _styleName      = "variable identifier"
                      , _styleStart     = lower
                      }

conStyle :: TokenParsing m => IdentifierStyle m
conStyle = identStyle { _styleName      = "constructor identifier"
                      , _styleStart     = upper
                      }

typeVarStyle :: TokenParsing m => IdentifierStyle m
typeVarStyle = identStyle { _styleName      = "type variable identifier"
                          , _styleStart     = lower
                          }


typeStyle :: TokenParsing m => IdentifierStyle m
typeStyle = identStyle { _styleName      = "type identifier"
                       , _styleStart     = upper
                       }


opStyle :: TokenParsing m => IdentifierStyle m
opStyle = IdentifierStyle
    { _styleName     = "operator"
    , _styleStart    = opStart
    , _styleLetter   = opLetter
    , _styleReserved = HashSet.fromList [":","::","..","...","=","\\","|","<-","->","@","~","=>"]
    , _styleHighlight = Operator
    , _styleReservedHighlight = ReservedOperator
    }

--identifier :: (TokenParsing m, Monad m) => m String
--identifier = ident identStyle


varIdent :: (TokenParsing m, Monad m) => m String
varIdent = ident varStyle

conIdent :: (TokenParsing m, Monad m) => m String
conIdent = ident conStyle

typeVarIdent :: (TokenParsing m, Monad m) => m String
typeVarIdent = ident typeVarStyle

typeIdent :: (TokenParsing m, Monad m) => m String
typeIdent = ident typeStyle

reservedIdent = reserve identStyle
reservedOp    = reserve opStyle

identLetter  = alphaNum <|> char '_'


wildcard    = symbolic  '_'   <?> "wildcard"
recWildcard = symbol    "..." <?> "record wildcard"
blockBegin  = symbolic  ':'
separator   = symbolic  ','
parenL      = symbolic  '('
parenR      = symbolic  ')'
bracketL    = symbolic  '['
bracketR    = symbolic  ']'
braceL      = symbolic  '{'
braceR      = symbolic  '}'
pipe        = symbolic  '|'
accessor    = symbolic  '.'
arrow       = symbol    "->"
typeDecl    = symbol    "::"
importAll   = symbolic  '*'
assignment  = symbolic  '='
nativeSym   = symbol    "```"
range       = symbol    ".."
ref         = symbolic  '@'



kwAlias     = reservedIdent "alias"
kwAs        = reservedIdent "as"
kwCase      = reservedIdent "case"
kwClass     = reservedIdent "class"
kwDef       = reservedIdent "def"
kwElse      = reservedIdent "else"
kwFrom      = reservedIdent "from"
kwIf        = reservedIdent "if"
kwInterface = reservedIdent "interface"
kwImport    = reservedIdent "import"
kwType      = reservedIdent "type"



----------------------------------------------------------------------
-- Numbers
----------------------------------------------------------------------

numberL = sign <**> numBase

numBase =   (numPrefix ['o', 'O'] *> (Number.octodecimal <$> numRepr octDigit <*> numExp 'e'))
        <|> (numPrefix ['x', 'X'] *> (Number.hexadecimal <$> numRepr hexDigit <*> numExp 'p'))
        <|> (Number.decimal <$> numRepr digit <*> numExp 'e')

numPrefix pfxs = try (char '0' *> choice (fmap char pfxs))

numRepr baseDigit = some baseDigit <**> ((flip Number.Float <$ char '.' <*> some baseDigit) <|> pure Number.Decimal)

numExp c = (Just <$ char c <*> numberL) <|> pure Nothing

sign = highlight Operator
     $ Number.Negative <$ char '-'
   <|> Number.Positive <$ char '+'
   <|> pure Number.Positive

-- c-pasted
number :: TokenParsing m => Integer -> m Char -> m Integer
number base baseDigit =
  foldl' (\x d -> base*x + toInteger (digitToInt d)) 0 <$> some baseDigit

----------------------------------------------------------------------
-- Strings
----------------------------------------------------------------------

stringLiteral :: (TokenParsing m, IsString s) => m s
stringLiteral = fromString <$> token (highlight StringLiteral lit) where
  lit = Prelude.foldr (maybe id (:)) ""
    <$> between (char '"') (char '"' <?> "end of string") (many stringChar)
    <?> "string"
  stringChar = Just <$> stringLetter
           <|> stringEscape
       <?> "string character"
  stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026' || c == '\n' || c == '\r'))

  stringEscape = highlight EscapeCode $ char '\\' *> esc where
    esc = Nothing <$ escapeGap
      <|> Nothing <$ escapeEmpty
      <|> Just <$> escapeCode
  escapeEmpty = char '&'
  escapeGap = skipSome space *> (char '\\' <?> "end of string gap")
{-# INLINE stringLiteral #-}


-- c-pasted
escapeCode :: TokenParsing m => m Char
escapeCode = (charEsc <|> charNum <|> charAscii <|> charControl) <?> "escape code"
  where
  charControl = (\c -> toEnum (fromEnum c - fromEnum '@')) <$> (char '^' *> (upper <|> char '@'))
  charNum     = toEnum . fromInteger <$> num where
    num = decimal
      <|> (char 'o' *> number 8 octDigit)
      <|> (char 'x' *> number 16 hexDigit)
  charEsc = choice $ parseEsc <$> escMap
  parseEsc (c,code) = code <$ char c
  escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"
  charAscii = choice $ parseAscii <$> asciiMap
  parseAscii (asc,code) = try $ code <$ string asc
  asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)
  ascii2codes, ascii3codes :: [String]
  ascii2codes = [ "BS","HT","LF","VT","FF","CR","SO"
                , "SI","EM","FS","GS","RS","US","SP"]
  ascii3codes = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK"
                ,"BEL","DLE","DC1","DC2","DC3","DC4","NAK"
                ,"SYN","ETB","CAN","SUB","ESC","DEL"]
  ascii2, ascii3 :: String
  ascii2 = "\BS\HT\LF\VT\FF\CR\SO\SI\EM\FS\GS\RS\US\SP"
  ascii3 = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\BEL\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\SUB\ESC\DEL"