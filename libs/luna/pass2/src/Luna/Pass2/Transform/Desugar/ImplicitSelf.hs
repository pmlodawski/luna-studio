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


module Luna.Pass2.Transform.Desugar.ImplicitSelf where

import           Flowbox.Prelude              hiding (Traversal)
import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join, mapM, State)
import qualified Luna.ASTNew.Traversals       as AST
import qualified Luna.ASTNew.Enum             as Enum
import           Luna.ASTNew.Enum             (Enumerated, IDTag(IDTag))
import qualified Luna.ASTNew.Decl             as Decl
import           Luna.ASTNew.Decl             (LDecl, Field(Field))
import qualified Luna.ASTNew.Module           as Module
import           Luna.ASTNew.Module           (Module(Module), LModule)
import           Luna.ASTNew.Unit             (Unit(Unit))
import qualified Luna.ASTNew.Label            as Label
import           Luna.ASTNew.Label            (Label(Label))
import qualified Luna.ASTNew.Type             as Type
import           Luna.ASTNew.Type             (Type)
import qualified Luna.ASTNew.Pat              as Pat
import           Luna.ASTNew.Pat              (LPat, Pat)
import           Luna.ASTNew.Expr             (LExpr, Expr)
import qualified Luna.ASTNew.Lit              as Lit
import qualified Luna.ASTNew.Native           as Native
import qualified Luna.ASTNew.Name             as Name
import           Luna.ASTNew.Name             (TName(TName), TVName(TVName))
import           Luna.Pass                    (Pass(Pass), PassMonad, PassCtx)
import qualified Luna.Pass                    as Pass

import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)

import           Luna.Data.ASTInfo            (ASTInfo, genID)

import qualified Luna.Data.Namespace.State    as State 
import qualified Luna.Parser.Parser           as Parser
import qualified Luna.Parser.State            as ParserState
import           Luna.ASTNew.Name.Pattern     (NamePat(NamePat), Segment(Segment), Arg(Arg))

----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data ImplSelf = ImplSelf

type ISPass                 m   = PassMonad ASTInfo m
type ISCtx              lab m a = (Enumerated lab, ISTraversal m a)
type ISTraversal            m a = (PassCtx m, AST.Traversal        ImplSelf (ISPass m) a a)
type ISDefaultTraversal     m a = (PassCtx m, AST.DefaultTraversal ImplSelf (ISPass m) a a)


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

isDecl :: (ISCtx lab m a) => (LDecl lab a) -> ISPass m (LDecl lab a)
isDecl e@(Label lab expr) = case expr of
    Decl.Function path sig output body -> do
        argId <- genID
        let NamePat pfx (Segment name args) segs = sig
            selfArg = Arg (Label (Enum.tag argId) $ Pat.Var "self") Nothing
            nsig    = NamePat pfx (Segment name $ selfArg : args) segs
        return $ Label lab $ Decl.Function path nsig output body
    _ -> continue
    where id       = Enum.id lab
          continue = defaultTraverseM e 


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance ISCtx lab m a => AST.Traversal ImplSelf (ISPass m) (LDecl lab a) (LDecl lab a) where
    traverseM _ = isDecl

