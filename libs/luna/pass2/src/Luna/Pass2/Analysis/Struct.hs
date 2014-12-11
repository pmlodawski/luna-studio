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

{-# LANGUAGE TypeFamilies #-}

module Luna.Pass2.Analysis.Struct where

import           Flowbox.Prelude
import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join, mapM, State)
import qualified Luna.ASTNew.Traversals       as AST
import qualified Luna.ASTNew.Enum             as Enum
import           Luna.ASTNew.Enum             (Enumerated, IDTag(IDTag))
import qualified Luna.ASTNew.Decl             as Decl
import           Luna.ASTNew.Decl             (LDecl, Field(Field))
import qualified Luna.ASTNew.Module           as Module
import           Luna.ASTNew.Module           (Module(Module), LModule)
import           Luna.ASTNew.Unit             (Unit(Unit))
import           Luna.ASTNew.NameBase         (NameBase(nameBase))
import qualified Luna.ASTNew.Label            as Label
import           Luna.ASTNew.Label            (Label(Label))
import qualified Luna.ASTNew.Type             as Type
import           Luna.ASTNew.Type             (Type)
import           Luna.ASTNew.Expr             (LExpr, Expr)
import qualified Luna.ASTNew.Expr             as Expr
import qualified Luna.ASTNew.Pat              as Pat
import           Luna.ASTNew.Pat              (LPat, Pat)
import qualified Luna.ASTNew.Lit              as Lit
import           Luna.ASTNew.Arg              (Arg(Arg))
import qualified Luna.ASTNew.Native           as Native
import           Luna.ASTNew.Name.Multi       (MultiName(MultiName))
import qualified Luna.ASTNew.Name.Multi       as MultiName
import qualified Luna.ASTNew.Name             as Name
import           Luna.ASTNew.Name             (TName(TName), TVName(TVName))
import           Luna.Pass                    (Pass(Pass), PassMonad, PassCtx)
import qualified Luna.Pass                    as Pass

import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)

import           Luna.Data.StructInfo         (StructInfo)

import qualified Luna.Data.Namespace.State    as State 
import           Luna.Data.Namespace.State    (regAlias, regParent, regVarName, regNamePattern, regTypeName, withNewScope)
import qualified Luna.Parser.State            as ParserState
import qualified Luna.ASTNew.Name.Pattern     as NamePattern

----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data StructAnalysis = StructAnalysis

type SAPass                 m   = PassMonad Namespace m
type SACtx              lab m a = (Enumerated lab, SATraversal m a)
type SATraversal            m a = (PassCtx m, AST.Traversal        StructAnalysis (SAPass m) a a)
type SADefaultTraversal     m a = (PassCtx m, AST.DefaultTraversal StructAnalysis (SAPass m) a a)

----------------------------------------------------------------------
-- Utils functions
----------------------------------------------------------------------

traverseM :: (SATraversal m a) => a -> SAPass m a
traverseM = AST.traverseM StructAnalysis

defaultTraverseM :: (SADefaultTraversal m a) => a -> SAPass m a
defaultTraverseM = AST.defaultTraverseM StructAnalysis

----------------------------------------------------------------------
-- Pass functions
----------------------------------------------------------------------

pass :: (Monoid s, SADefaultTraversal m a) => Pass s (a -> SAPass m StructInfo)
pass = Pass "Alias analysis" 
            "Basic alias analysis that results in scope, alias, orphans and parent mapping information" 
            mempty aaUnit

aaUnit :: SADefaultTraversal m a => a -> SAPass m StructInfo
aaUnit ast = defaultTraverseM ast *> (view Namespace.info <$> get)

aaMod :: SACtx lab m a => LModule lab a -> SAPass m (LModule lab a)
aaMod mod@(Label lab (Module path name body)) = withNewScope id continue
    where continue =  registerDecls body
                   *> defaultTraverseM mod
          id       = Enum.id lab

aaPat :: (PassCtx m, Enumerated lab) => LPat lab -> SAPass m (LPat lab)
aaPat p@(Label lab pat) = case pat of
    Pat.Var         name       -> regVarName id (Name.fromName name)
                                  *> regParent id
                                  *> continue
    _                          -> continue
    where id = Enum.id lab
          continue = defaultTraverseM p 

aaDecl :: SACtx lab m a => (LDecl lab a) -> SAPass m (LDecl lab a)
aaDecl d@(Label lab decl) = case decl of
    Decl.Function path name inputs output body -> withNewScope id $ defaultTraverseM d
    _                                          -> continue
    where id       = Enum.id lab
          continue = defaultTraverseM d

-- FIXME [wd]: remove the assumption that a is MultiName. variables should always contain name as MultiName!
aaExpr :: (SACtx lab m a, a~MultiName) => (LExpr lab a) -> SAPass m (LExpr lab a)
aaExpr e@(Label lab expr) = case expr of
    var@(Expr.Var name)     -> regParent id
                               *> regAlias id name
                               *> continue
    _                       -> continue
    where id       = Enum.id lab
          continue = defaultTraverseM e

registerDecls :: SACtx lab m a => [LDecl lab a] -> SAPass m ()
registerDecls decls =  mapM_ registerHeaders  decls
                    *> mapM_ registerDataDecl decls

registerDataDecl :: SACtx lab m a => LDecl lab a -> SAPass m ()
registerDataDecl (Label lab decl) = case decl of
    Decl.Data     name _ cons defs   -> withNewScope id (registerDecls defs) *> pure ()
    _                                -> pure ()
    where id = Enum.id lab

registerHeaders :: SACtx lab m a => LDecl lab a -> SAPass m ()
registerHeaders (Label lab decl) = case decl of
    Decl.Function _ namePat inputs _ _  -> regVarName id (NamePattern.toName namePat)
                                        <* regNamePattern id namePat
                                        <* withNewScope id (defaultTraverseM inputs)
    Decl.Data     name _ cons _         -> regTypeName id (Name.fromName name) 
                                        <* mapM_ registerCons cons
    _                                   -> pure ()
    where id = Enum.id lab
          registerCons (Label lab (Decl.Cons name fields)) = regVarName (Enum.id lab) (Name.fromName name)


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance SACtx lab m a => AST.Traversal StructAnalysis (SAPass m) (LModule lab a) (LModule lab a) where
    traverseM _ = aaMod

instance SACtx lab m a => AST.Traversal StructAnalysis (SAPass m) (LDecl lab a) (LDecl lab a) where
    traverseM _ = aaDecl

instance (SACtx lab m v, v~MultiName) => AST.Traversal StructAnalysis (SAPass m) (LExpr lab v) (LExpr lab v) where
    traverseM _ = aaExpr

instance (PassCtx m, Enumerated lab) => AST.Traversal StructAnalysis (SAPass m) (LPat lab) (LPat lab) where
    traverseM _ = aaPat