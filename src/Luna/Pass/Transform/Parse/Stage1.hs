---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}

module Luna.Pass.Transform.Parse.Stage1 where

import           Flowbox.Prelude              hiding (Traversal)
import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join, mapM, State)
import qualified Luna.Syntax.Traversals       as AST
import qualified Luna.Syntax.Enum       as Enum
import           Luna.Syntax.Enum       (Enumerated, IDTag(IDTag))
import qualified Luna.Syntax.Decl   as Decl
import           Luna.Syntax.Decl   (LDecl, Field(Field))
import qualified Luna.Syntax.Module as Module
import           Luna.Syntax.Module (Module(Module), LModule)
import           Luna.Syntax.Unit   (Unit(Unit))
import qualified Luna.Syntax.Label  as Label
import           Luna.Syntax.Label  (Label(Label))
import qualified Luna.Syntax.Type   as Type
import           Luna.Syntax.Type   (Type)
import qualified Luna.Syntax.Pat    as Pat
import           Luna.Syntax.Pat    (LPat, Pat)
import           Luna.Syntax.Expr   (LExpr, Expr)
import qualified Luna.Syntax.Lit    as Lit
import           Luna.Syntax.Arg    (Arg(Arg))
import qualified Luna.Syntax.Native as Native
import qualified Luna.Syntax.Name             as Name
import           Luna.Syntax.Name             (TName(TName), TVName(TVName))
import           Luna.Pass              (Pass(Pass), PassMonad, PassCtx)
import qualified Luna.Pass              as Pass

import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)

import           Luna.Data.ASTInfo            (ASTInfo)

import qualified Luna.Data.Namespace.State    as State 
import qualified Luna.Parser.Parser           as Parser
import qualified Luna.Parser.State            as ParserState
import           Data.ByteString              (ByteString)
import           Text.PrettyPrint.ANSI.Leijen (Doc, displayS, renderPretty)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)
import Luna.Data.Source (Source(Source), SourceReader, Code(Code))
import qualified Luna.Data.Source as Source
import           Data.String             (IsString, fromString)

----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data Stage1 = Stage1

type Stage1Pass             m     = PassMonad () m
type Stage1Ctx              lab m = (Enumerated lab, PassCtx m)
type Stage1Traversal        m a b = (PassCtx m, AST.Traversal        Stage1 (Stage1Pass m) a b)
type Stage1DefaultTraversal m a b = (PassCtx m, AST.DefaultTraversal Stage1 (Stage1Pass m) a b)


------------------------------------------------------------------------
---- Pass functions
------------------------------------------------------------------------

pass :: (MonadIO m, SourceReader (Stage1Pass m) a, PassCtx m) => Pass () (Source a -> Stage1Pass m (Unit (LModule IDTag String), ASTInfo))
pass = Pass "Parser stage-1" "Parses declarations without parsing expressions" ()
       passRunner

passRunner src = do
    (Source name (Code code)) <- Source.read src
    result <- lift . hoistEither . (tmpFixErrorParse (Parser.moduleParser [name] Parser.defState)) $ code
    let astinfo = view ParserState.info $ snd result
    return $ (fst result, astinfo)

tmpFixErrorParse a b = case Parser.parseText2 a b of
    Left doc -> Left $ showWidth 40 doc
    Right r  -> Right r

showWidth :: Int -> Doc -> String
showWidth w x   = displayS (renderPretty 0.4 w x) ""