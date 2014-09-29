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
import           Text.Parser.Token
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

reserved   = reserve identStyle

identLetter  = alphaNum <|> char '_'

kwAlias     = reserved "alias"
kwAs        = reserved "as"
kwCase      = reserved "case"
kwClass     = reserved "class"
kwDef       = reserved "def"
kwElse      = reserved "else"
kwFrom      = reserved "from"
kwIf        = reserved "if"
kwInterface = reserved "interface"
kwImport    = reserved "import"
kwType      = reserved "type"



accessor  = token $ char '.'
separator = token $ char ','
