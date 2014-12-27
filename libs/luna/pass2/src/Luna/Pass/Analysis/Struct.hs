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

module Luna.Pass.Analysis.Struct where

import           Flowbox.Prelude
import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join, mapM, State)
import qualified Luna.Syntax.Traversals       as AST
import qualified Luna.Syntax.Enum             as Enum
import           Luna.Syntax.Enum             (Enumerated, IDTag(IDTag))
import qualified Luna.Syntax.Decl             as Decl
import           Luna.Syntax.Decl             (LDecl, Field(Field))
import qualified Luna.Syntax.Module           as Module
import           Luna.Syntax.Module           (Module(Module), LModule)
import           Luna.Syntax.Unit             (Unit(Unit))
import qualified Luna.Syntax.Label            as Label
import           Luna.Syntax.Label            (Label(Label))
import qualified Luna.Syntax.Type             as Type
import           Luna.Syntax.Type             (Type)
import           Luna.Syntax.Expr             (LExpr, Expr)
import qualified Luna.Syntax.Expr             as Expr
import qualified Luna.Syntax.Pat              as Pat
import           Luna.Syntax.Pat              (LPat, Pat)
import qualified Luna.Syntax.Lit              as Lit
import           Luna.Syntax.Arg              (Arg(Arg))
import qualified Luna.Syntax.Native           as Native
import           Luna.Syntax.Name.Path        (NamePath(NamePath))
import qualified Luna.Syntax.Name.Path        as NamePath
import qualified Luna.Syntax.Name             as Name
import           Luna.Syntax.Name             (TName(TName), TVName(TVName))
import           Luna.Pass                    (Pass(Pass), PassMonad, PassCtx)
import qualified Luna.Pass                    as Pass

import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)

import           Luna.Data.StructInfo         (StructInfo, OriginInfo(OriginInfo))

import qualified Luna.Data.Namespace.State    as State 
import           Luna.Data.Namespace.State    (regAlias, regParent, regVarName, regNamePatDesc, regTypeName, withNewScope)
import qualified Luna.Parser.State            as ParserState
--import qualified Luna.Syntax.Name.Pattern     as NamePattern
import qualified Luna.Syntax.Name.Pattern     as NamePattern
import           Luna.Syntax.Foreign          (Foreign(Foreign))

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