{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Combinators where

import           Control.Applicative
import           Control.Exception            (bracket)
import           Control.Monad.State          hiding ((<$!>))
import qualified Data.ByteString              as B
import           Data.ByteString.UTF8         as UTF8 hiding (length, foldr)
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



checkIf f msg p = do
        obj <- p
        if (f obj)
                then unexpected (msg ++ show obj)
                else return obj

pl <$*> pr = do
    n <- pr
    pl n

pl <*$> pr = do
    n <- pl
    pr n

infixl 5 <?*>
infixl 4 <??>
infixl 4 <**$>
infixl 4 <??$>
p <?*> q = (p <*> q) <|> q
-- p <**> q = (\f g -> g f) <$> p <*> q
p <??> q = p <**> (q <|> return id)
p <**$> q = p <**> (flip (foldr ($)) <$> q)
p <??$> q = p <**> ((flip (foldr ($)) <$> q) <|> return id)


sepBy2 p sep = (:) <$> p <* sep <*> sepBy1 p sep

sepBy_ng  p sep = sepBy1_ng p sep <|> return []
sepBy1_ng p sep = (:) <$> p <*> many (try (sep *> p))
sepBy2_ng p sep = (:) <$> p <*> try(sep *> sepBy1_ng p sep)


many1 p = (:) <$> p <*> many p


maybe p = try (Just <$> p) <|> pure Nothing


applyAll x (f : fs) = applyAll (f x) fs
applyAll x [] = x