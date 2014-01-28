---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Luna.Passes.Transform.AST.TxtParser.Utils where

import           Control.Applicative
import qualified Text.Parsec         as Parsec

import           Flowbox.Luna.Data.AST.SourcePos                   (SourcePos (SourcePos))
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Token as Token
import           Flowbox.Prelude
import           Text.Parsec                                       hiding (getPosition, many, optional, parse, (<|>))
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.ParseState as ParseState

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

--sepBy2  p sep = (:) <$> p <*> try(sep *> sepBy1 p sep)

sepBy_ng  p sep = sepBy1_ng p sep <|> return []
sepBy1_ng p sep = (:) <$> p <*> many (try(sep *> p))
sepBy2_ng p sep = (:) <$> p <*> try(sep *> sepBy1_ng p sep)

liftList p = (:[]) <$> p


applyAll x (f : fs) = applyAll (f x) fs
applyAll x [] = x



getPosition = convertSourcePos <$> Parsec.getPosition
convertSourcePos psp = SourcePos (sourceLine psp) (sourceColumn psp)

storePos p = (\pre res post -> Token.mk res pre post) <$> getPosition <*> p <*> getPosition


pLastLexeme = view ParseState.lastLexeme <$> getState

pLastLexemeEmpty = do
    llex <- pLastLexeme
    case llex of
        [] -> return ()
        _  -> parserFail "not empty"