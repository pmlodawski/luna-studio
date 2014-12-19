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

module Luna.Pass2.Transform.Parse.Stage1 where

import           Flowbox.Prelude              hiding (Traversal)
import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join, mapM, State)
import qualified Luna.ASTNew.Traversals       as AST
import qualified Luna.ASTNew.Enum       as Enum
import           Luna.ASTNew.Enum       (Enumerated, IDTag(IDTag))
import qualified Luna.ASTNew.Decl   as Decl
import           Luna.ASTNew.Decl   (LDecl, Field(Field))
import qualified Luna.ASTNew.Module as Module
import           Luna.ASTNew.Module (Module(Module), LModule)
import           Luna.ASTNew.Unit   (Unit(Unit))
import qualified Luna.ASTNew.Label  as Label
import           Luna.ASTNew.Label  (Label(Label))
import qualified Luna.ASTNew.Type   as Type
import           Luna.ASTNew.Type   (Type)
import qualified Luna.ASTNew.Pat    as Pat
import           Luna.ASTNew.Pat    (LPat, Pat)
import           Luna.ASTNew.Expr   (LExpr, Expr)
import qualified Luna.ASTNew.Lit    as Lit
import           Luna.ASTNew.Arg    (Arg(Arg))
import qualified Luna.ASTNew.Native as Native
import qualified Luna.ASTNew.Name             as Name
import           Luna.ASTNew.Name             (TName(TName), TVName(TVName))
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
import Luna.Data.Source (Source(Source), Medium, Code(Code))
import qualified Luna.Data.Source as Source
import           Data.String             (IsString, fromString)


----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data Stage2 = Stage2

type Stage2Pass             m     = PassMonad () m
type Stage2Ctx              lab m = (Enumerated lab, PassCtx m)
type Stage2Traversal        m a b = (PassCtx m, AST.Traversal        Stage2 (Stage2Pass m) a b)
type Stage2DefaultTraversal m a b = (PassCtx m, AST.DefaultTraversal Stage2 (Stage2Pass m) a b)


------------------------------------------------------------------------
---- Pass functions
------------------------------------------------------------------------

pass :: MonadIO m => Pass () (Source Medium -> Stage2Pass m (Unit (LModule IDTag String), ASTInfo))
pass = Pass "Parser stage-2" "Parses expressions based on AST stage-1 and alias analysis" ()
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