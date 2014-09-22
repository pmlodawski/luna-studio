{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Lexer where

import           Control.Applicative
import           Control.Exception            (bracket)
import           Control.Monad.State          hiding ((<$!>))
import qualified Data.ByteString              as B
import           Data.ByteString.UTF8         as UTF8 hiding (length)
import           Data.CharSet.ByteSet         as S
import qualified Data.HashSet                 as HashSet
import           Flowbox.Prelude
import           System.Environment           (getArgs)
import           System.IO                    (IOMode (ReadMode), hClose, openFile)
import           System.IO                    (stdout)
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.PrettyPrint.ANSI.Leijen (displayIO, linebreak, renderPretty, (<>))
import           Text.Trifecta                hiding (token)
import           Text.Trifecta.Delta          as Delta





reservedIdents :: [String]
reservedIdents = ["alias", "as", "case", "class", "def", "else", "from", "if", "interface", "import", "in", "type"]

identStyle = IdentifierStyle
  { _styleName      = "identifier"
  , _styleStart     = letter <|> char '_'
  , _styleLetter    = alphaNum <|> oneOf "_'"
  , _styleReserved  = HashSet.fromList reservedIdents
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

variableStyle :: TokenParsing m => IdentifierStyle m
variableStyle = identStyle { _styleName      = "variable",
                             _styleStart     = lower
                           }


identifier :: (TokenParsing m, Monad m) => m String
identifier = ident identStyle

variable :: (TokenParsing m, Monad m) => m String
variable = ident variableStyle

reserved   = reserve identStyle


identLetter  = alphaNum <|> char '_'

pTypeAlias  = reserved "alias"
pAs         = reserved "as"
pCase       = reserved "case"
pClass      = reserved "class"
pDef        = reserved "def"
pElse       = reserved "else"
pFrom       = reserved "from"
pIf         = reserved "if"
pInterface  = reserved "interface"
pImport     = reserved "import"
pTypeDef    = reserved "type"
