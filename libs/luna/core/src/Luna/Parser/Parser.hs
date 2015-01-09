{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE DeriveDataTypeable        #-}

{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Luna.Parser.Parser
-- Copyright   :  (C) 2014 Flowbox
-- License     :  AllRightsReserved
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module Luna.Parser.Parser where

import Flowbox.Prelude hiding (init)

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
--import qualified Luna.Parser.Pragma as Pragma
import qualified Luna.Parser.Indent as Indent

import qualified Data.List as List

import qualified Luna.Parser.Type    as Type
import qualified Luna.Parser.Pattern as Pattern
import qualified Luna.Parser.Literal as Literal
import qualified Luna.Parser.Struct  as Struct
import qualified Luna.Parser.Term    as Term
import qualified Luna.Parser.Decl    as Decl
import qualified Luna.Parser.Module  as Module

import Luna.Parser.Builder (labeled, label, nextID, qualifiedPath, withLabeled)
import           Luna.System.Session (registerPragma, enablePragma, disablePragma)
import           Luna.System.Pragma  (pragma, SwitchPragma, IsPragma)


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

appConf = id

-- FIXME[wd]: logika powina byc przeniesiona na system pluginow
defConfig = appConf def
-- FIXME[wd]: debugowo ustawione wartosci typow
emptyState = def :: ParserState
defState  = emptyState


--st = def {State._conf = conf}




-----------------------------------------------------------
-- Section parsing
-----------------------------------------------------------
-- Usage example: parseExpr (fileFeed "test.txt")
parseGen p st = run (bundleResult (Module.unit p)) st
parseGen2 p st = run (bundleResult p) st

--moduleParser modPath = parseGen (upToEnd $ func)
moduleParser modPath = parseGen (upToEnd $ Module.pUnit $ Module.pModule (List.last modPath) (List.init modPath))
--exprParser           = parseGen (upToEnd expr)
exprBlockParser      = parseGen (upToEnd $ Struct.indBlock Term.expr)
exprBlockParser2     = parseGen2 (upToEnd $ Struct.indBlock Term.expr)
exprParser2          = parseGen2 (upToEnd Term.expr)
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


-----------------------------------------------------------
-- Pragmas & initialization
-----------------------------------------------------------

data ImplicitSelf = ImplicitSelf deriving (Show, Read, Typeable)
instance IsPragma ImplicitSelf
implicitSelf = pragma :: SwitchPragma ImplicitSelf


data OrphanNames  = OrphanNames   deriving (Show, Read, Typeable)
instance IsPragma OrphanNames
orphanNames  = pragma :: SwitchPragma OrphanNames


init = do
    registerPragma implicitSelf
    registerPragma orphanNames

    enablePragma   implicitSelf
    disablePragma  orphanNames