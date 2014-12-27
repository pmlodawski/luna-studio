---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE DeriveDataTypeable        #-}

{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}

module Luna.Parser.Parser where

import Flowbox.Prelude 

import           Text.Parser.Combinators 
import qualified Data.ByteString.UTF8         as UTF8
import Control.Monad.State (get, evalStateT)

import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.ByteString as ByteStr
import           Text.PrettyPrint.ANSI.Leijen (displayIO, linebreak, renderPretty)
import qualified Text.PrettyPrint.ANSI.Leijen as Leijen

import           Text.Trifecta.Delta (Delta(Directed))
import           Text.Trifecta.Result (Result(Failure, Success))
import qualified Text.Trifecta.Parser as Trifecta

import qualified Luna.Parser.State  as ParserState
import           Luna.Parser.State  (ParserState)
import qualified Luna.Parser.Token  as Tok
import qualified Luna.Data.Config   as Config
import qualified Luna.Pragma.Pragma as Pragma
import qualified Luna.Parser.Pragma as Pragma
import qualified Luna.Parser.Indent as Indent


import Luna.Parser.Type
import Luna.Parser.Pattern
import Luna.Parser.Literal
import Luna.Parser.Struct
import Luna.Parser.Term
import Luna.Parser.Decl
import Luna.Parser.Module

import Luna.Parser.Builder (labeled, label, nextID, qualifiedPath, withLabeled)




-----------------------------------------------------------
-- Utils
-----------------------------------------------------------

parserName = "Luna Compiler"

run p st = evalStateT (Indent.parser p) st

handleResult r = case r of
    Failure e -> Left e
    Success a -> Right a

bundleResult p = (,) <$> p <*> get

end = (Tok.spaces <?> "") <* (eof <?> "")

upToEnd p = Tok.spaces *> p <* end

renderErr e = renderPretty 0.8 80 $ e Leijen.<> linebreak

-----------------------------------------------------------
-- Pragmas
-----------------------------------------------------------

appConf = Config.registerPragma (undefined :: Pragma.TabLength)
        . Config.registerPragma (undefined :: Pragma.AllowOrphans)
        . Config.registerPragma (undefined :: Pragma.ImplicitSelf)

-- FIXME[wd]: logika powina byc przeniesiona na system pluginow
defConfig = appConf def
-- FIXME[wd]: debugowo ustawione wartosci typow
emptyState = def :: ParserState ()
defState  = emptyState & ParserState.conf .~ defConfig


appSt = ParserState.conf %~ appConf

--st = def {State._conf = conf}

-----------------------------------------------------------
-- Section parsing
-----------------------------------------------------------
-- Usage example: parseExpr (fileFeed "test.txt")
parseGen p st = run (bundleResult (unit p)) st
parseGen2 p st = run (bundleResult p) st

--moduleParser modPath = parseGen (upToEnd $ func)
moduleParser modPath = parseGen (upToEnd $ pUnit $ pModule (last modPath) (init modPath))
--exprParser           = parseGen (upToEnd expr)
exprBlockParser      = parseGen (upToEnd $ indBlock expr)
exprBlockParser2     = parseGen2 (upToEnd $ indBlock expr)
exprParser2          = parseGen2 (upToEnd expr)
--patternParser        = parseGen (upToEnd pattern)
--typeParser           = parseGen (upToEnd typeT)

-----------------------------------------------------------
-- Input utils
-----------------------------------------------------------

parserDelta name = Directed (UTF8.fromString name) 0 0 0 0

parseFromByteString = Trifecta.parseByteString

parseFromText p delta txt = Trifecta.parseByteString p delta (convert $ Text.encodeUtf8 txt)

parseFromString p delta input = parseFromByteString p delta (UTF8.fromString input)

parseFromFile p delta path = do
  s <- liftIO $ ByteStr.readFile path
  return $ parseFromByteString p delta s

parseFile       path  p = handleResult <$> parseFromFile       p (parserDelta parserName) path
parseString     input p = handleResult  $  parseFromString     p (parserDelta parserName) input
parseByteString input p = handleResult  $  parseFromByteString p (parserDelta parserName) input

parseByteString2 p input = handleResult  $  parseFromByteString p (parserDelta parserName) input
parseText2 p input = handleResult  $  parseFromText p (parserDelta parserName) input
                --data AliasAnalysis = AliasAnalysis

                --traverseM        = AST.traverseM        AliasAnalysis
                --defaultTraverseM = AST.defaultTraverseM AliasAnalysis

testme ast st = ast -- runState (traverseM ast) st
