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
{-# LANGUAGE OverloadedStrings #-}

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
import           Luna.ASTNew.Name.Path        (NamePath(NamePath))
import qualified Luna.ASTNew.Name.Path        as NamePath
import qualified Luna.ASTNew.Name             as Name
import           Luna.ASTNew.Name             (TName(TName), TVName(TVName))
import           Luna.Pass                    (Pass(Pass), PassMonad, PassCtx)
import qualified Luna.Pass                    as Pass

import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)

import           Luna.Data.StructInfo         (StructInfo, OriginInfo(OriginInfo))

import qualified Luna.Data.Namespace.State    as State 
import           Luna.Data.Namespace.State    (regAlias, regParent, regVarName, regNamePatDesc, regTypeName, withNewScope)
import qualified Luna.Parser.State            as ParserState
--import qualified Luna.ASTNew.Name.Pattern     as NamePattern
import qualified Luna.ASTNew.Name.Pattern     as NamePattern
import           Luna.ASTNew.Foreign          (Foreign(Foreign))

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
aaMod mod@(Label lab (Module _ body)) = withNewScope id continue
    where continue =  registerDecls body
                   *> defaultTraverseM mod
          id       = Enum.id lab

aaPat :: (PassCtx m, Enumerated lab) => LPat lab -> SAPass m (LPat lab)
aaPat p@(Label lab pat) = case pat of
    Pat.Var         name       -> regVarName (OriginInfo "dupa" id) (unwrap name)
                                  *> regParent id
                                  *> continue
    _                          -> continue
    where id = Enum.id lab
          continue = defaultTraverseM p 

aaDecl :: SACtx lab m a => (LDecl lab a) -> SAPass m (LDecl lab a)
aaDecl d@(Label lab decl) = case decl of
    Decl.Func {} -> withNewScope id $ defaultTraverseM d
    _            -> continue
    where id       = Enum.id lab
          continue = defaultTraverseM d

aaExpr :: SACtx lab m a => (LExpr lab a) -> SAPass m (LExpr lab a)
aaExpr e@(Label lab expr) = case expr of
    var@(Expr.Var (Expr.Variable name _)) -> regParent id
                                          *> regAlias id (unwrap name)
                                          *> continue
    cons@(Expr.Cons name)                 -> regParent id
                                          *> regAlias id (unwrap name)
                                          *> continue
    Expr.RecUpd name _                    -> regParent  id
                                          *> regAlias   id (unwrap name)
                                          *> regVarName (OriginInfo "dupa" id) (unwrap name)
                                          *> continue
    _                                     -> continue
    where id       = Enum.id lab
          continue = defaultTraverseM e


registerDecls :: SACtx lab m a => [LDecl lab a] -> SAPass m ()
registerDecls decls =  mapM_ registerHeaders  decls
                    *> mapM_ registerDataDecl decls

registerDataDecl :: SACtx lab m a => LDecl lab a -> SAPass m ()
registerDataDecl (Label lab decl) = case decl of
    Decl.Data (Decl.DataDecl name _ cons defs) -> withNewScope id (registerDecls defs) *> pure ()
    _                                          -> pure ()
    where id = Enum.id lab

registerHeaders :: SACtx lab m a => LDecl lab a -> SAPass m ()
registerHeaders (Label lab decl) = case decl of
    Decl.Func    fdecl -> regFuncDecl id fdecl
    Decl.Data    ddecl -> regDataDecl id ddecl
    Decl.Foreign fdecl -> regForeignDecl id fdecl
    _               -> pure ()
    where id = Enum.id lab
          
regForeignDecl id (Foreign tgt fdecl) = case fdecl of
    Decl.FData ddecl -> regDataDecl id ddecl
    Decl.FFunc fdecl -> regFuncDecl id fdecl

regFuncDecl id (Decl.FuncDecl _ sig _ _) =  regVarName (OriginInfo "dupa" id) (NamePattern.toNamePath sig)
                                         <* regNamePatDesc id (NamePattern.toDesc sig)

regDataDecl id (Decl.DataDecl name _ cons _) =  regTypeName (OriginInfo "dupa" id) (unwrap name) 
                                             <* mapM_ registerCons cons
    where registerCons (Label lab (Decl.Cons name fields)) = regVarName (OriginInfo "dupa" (Enum.id lab)) (unwrap name)

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance SACtx lab m a => AST.Traversal StructAnalysis (SAPass m) (LModule lab a) (LModule lab a) where
    traverseM _ = aaMod

instance SACtx lab m a => AST.Traversal StructAnalysis (SAPass m) (LDecl lab a) (LDecl lab a) where
    traverseM _ = aaDecl

instance SACtx lab m v => AST.Traversal StructAnalysis (SAPass m) (LExpr lab v) (LExpr lab v) where
    traverseM _ = aaExpr

instance (PassCtx m, Enumerated lab) => AST.Traversal StructAnalysis (SAPass m) (LPat lab) (LPat lab) where
    traverseM _ = aaPat