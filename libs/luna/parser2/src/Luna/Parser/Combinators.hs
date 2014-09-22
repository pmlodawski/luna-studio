{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Combinators where

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


sepBy2 p sep = (:) <$> p <* sep <*> sepBy1 p sep

sepBy_ng  p sep = sepBy1_ng p sep <|> return []
sepBy1_ng p sep = (:) <$> p <*> many (try (sep *> p))
sepBy2_ng p sep = (:) <$> p <*> try(sep *> sepBy1_ng p sep)