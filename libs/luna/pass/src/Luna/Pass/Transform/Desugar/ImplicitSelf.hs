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
{-# LANGUAGE OverloadedStrings #-}


module Luna.Pass.Transform.Desugar.ImplicitSelf where

import           Flowbox.Prelude              hiding (Traversal)
import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join, mapM, State)
import qualified Luna.Syntax.Traversals       as AST
import qualified Luna.Syntax.Enum             as Enum
import           Luna.Syntax.Enum             (Enumerated, IDTag(IDTag))
import qualified Luna.Syntax.Decl             as Decl
import           Luna.Syntax.Decl             (Decl, LDecl, Field(Field))
import qualified Luna.Syntax.Module           as Module
import           Luna.Syntax.Module           (Module(Module), LModule)
import           Luna.Syntax.Unit             (Unit(Unit))
import qualified Luna.Syntax.Label            as Label
import           Luna.Syntax.Label            (Label(Label))
import qualified Luna.Syntax.Type             as Type
import           Luna.Syntax.Type             (Type)
import qualified Luna.Syntax.Pat              as Pat
import           Luna.Syntax.Pat              (LPat, Pat)
import           Luna.Syntax.Expr             (LExpr, Expr)
import qualified Luna.Syntax.Lit              as Lit
import qualified Luna.Syntax.Native           as Native
import qualified Luna.Syntax.Name             as Name
import           Luna.Syntax.Name             (TName(TName), TVName(TVName))
import           Luna.Pass                    (Pass(Pass), PassMonad, PassCtx)
import qualified Luna.Pass                    as Pass

import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)

import           Luna.Data.ASTInfo            (ASTInfo, genID)

import qualified Luna.Data.Namespace.State    as State 
import qualified Luna.Parser.Parser           as Parser
import qualified Luna.Parser.State            as ParserState
import           Luna.Syntax.Arg              (Arg(Arg))
import           Luna.Syntax.Name.Pattern     (NamePat(NamePat), Segment(Segment))

----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data ImplSelf = ImplSelf

type ISPass                 m     = PassMonad ASTInfo m
type ISCtx                  m lab = (Monad m, Enumerated lab)
type ISTraversal            m a   = (PassCtx m, AST.Traversal        ImplSelf (ISPass m) a a)
type ISDefaultTraversal     m a   = (PassCtx m, AST.DefaultTraversal ImplSelf (ISPass m) a a)


------------------------------------------------------------------------
---- Utils functions
------------------------------------------------------------------------

traverseM :: (ISTraversal m a) => a -> ISPass m a
traverseM = AST.traverseM ImplSelf

defaultTraverseM :: (ISDefaultTraversal m a) => a -> ISPass m a
defaultTraverseM = AST.defaultTraverseM ImplSelf


------------------------------------------------------------------------
---- Pass functions
------------------------------------------------------------------------

pass :: ISDefaultTraversal m a => Pass ASTInfo (ASTInfo -> a -> ISPass m (a, ASTInfo))
pass = Pass "Implicit self" "Desugars AST by adding implicit self function parameters" undefined passRunner

passRunner astInfo ast = do
    put astInfo
    (,) <$> defaultTraverseM ast <*> get

isFuncDecl :: ISCtx m lab => Decl.FuncDecl lab e body -> ISPass m (Decl.FuncDecl lab e body)
isFuncDecl (Decl.FuncDecl path sig output body) = do
    argId <- genID
    let NamePat pfx (Segment name args) segs = sig
        selfArg = Arg (Label (Enum.tag argId) $ Pat.Var "self") Nothing
        nsig    = case pfx of
        	          Nothing    -> NamePat pfx (Segment name $ selfArg : args) segs
        	          Just parg  -> NamePat (Just selfArg) (Segment name $ parg : args) segs
    return $ Decl.FuncDecl path nsig output body

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance ISCtx m lab => AST.Traversal ImplSelf (ISPass m) (Decl.FuncDecl lab e body) (Decl.FuncDecl lab e body) where
    traverseM _ = isFuncDecl

