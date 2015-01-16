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


module Luna.Pass.Transform.Desugar.ImplicitCalls where

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
import qualified Luna.Syntax.Expr             as Expr
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
import           Luna.Data.StructInfo         (StructInfo)
import qualified Luna.Data.StructInfo         as StructInfo
import Control.Monad (join)

----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data ImplCalls = ImplCalls
data ImplCallsOmit = ImplCallsOmit

type ISPass                 m     = PassMonad ASTInfo m
type ISCtx              lab m a   = (Monad m, Enumerated lab, ISTraversal m a, ISTraversalOmit m a, Num lab)
type ISTraversal            m a   = (PassCtx m, AST.Traversal        ImplCalls (ISPass m) a a)
type ISTraversalOmit        m a   = (PassCtx m, AST.Traversal        ImplCallsOmit (ISPass m) a a)
type ISDefaultTraversal     m a   = (PassCtx m, AST.DefaultTraversal ImplCalls (ISPass m) a a)
type ISDefaultTraversalOmit m a   = (PassCtx m, AST.DefaultTraversal ImplCallsOmit (ISPass m) a a)


------------------------------------------------------------------------
---- Utils functions
------------------------------------------------------------------------

traverseM :: (ISTraversal m a) => a -> ISPass m a
traverseM = AST.traverseM ImplCalls

defaultTraverseM :: (ISDefaultTraversal m a) => a -> ISPass m a
defaultTraverseM = AST.defaultTraverseM ImplCalls

defaultTraverseOmitM :: (ISDefaultTraversalOmit m a) => a -> ISPass m a
defaultTraverseOmitM = AST.defaultTraverseM ImplCallsOmit


------------------------------------------------------------------------
---- Pass functions
------------------------------------------------------------------------

pass :: ISDefaultTraversal m a => Pass ASTInfo (ASTInfo -> a -> ISPass m (a, ASTInfo))
pass = Pass "Implicit calls" "Desugars AST by adding implicit calls" undefined passRunner

passRunner ai ast = do
    put ai
    (,) <$> defaultTraverseM ast <*> get

exprScopes :: ISCtx lab m a => LExpr lab a -> ISPass m (LExpr lab a)
exprScopes ast@(Label lab e) = case e of
    Expr.Cons     {} -> Label 999 <$> (Expr.app <$> continue <*> pure [])
    Expr.Accessor {} -> Label 999 <$> (Expr.app <$> continue <*> pure [])
                -- TODO [wd]: ^-- a magic constants :)
    Expr.App      {} -> defaultTraverseOmitM ast -- Label 999 <$> (Expr.app <$> defaultTraverseOmitM ast <*> pure [])
    _                -> continue
    where continue = defaultTraverseM ast
          id       = Enum.id lab


exprScopesOmit :: ISCtx lab m a => LExpr lab a -> ISPass m (LExpr lab a)
exprScopesOmit = defaultTraverseM

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance ISCtx lab m a => AST.Traversal ImplCalls (ISPass m) (LExpr lab a) (LExpr lab a) where
    traverseM _ = exprScopes


instance ISCtx lab m a => AST.Traversal ImplCallsOmit (ISPass m) (LExpr lab a) (LExpr lab a) where
    traverseM _ = exprScopesOmit

