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

module Luna.Pass2.Analysis.Alias where

import           Flowbox.Prelude
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
import qualified Luna.ASTNew.Lit    as Lit
import           Luna.ASTNew.Arg    (Arg(Arg))
import qualified Luna.ASTNew.Native as Native
import           Luna.ASTNew.Name.Multi       (MultiName(MultiName))
import qualified Luna.ASTNew.Name.Multi       as MultiName
import qualified Luna.ASTNew.Name             as Name
import           Luna.ASTNew.Name             (TName(TName), TVName(TVName))
import           Luna.Pass2.Pass              (Pass(Pass), PassMonad, PassCtx)
import qualified Luna.Pass2.Pass              as Pass

import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)

import           Luna.Data.AliasInfo          (AliasInfo)

import qualified Luna.Data.Namespace.State    as State 
import           Luna.Data.Namespace.State    (regVarName, regTypeName, withNewScope)
import qualified Luna.Parser.State            as ParserState


----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data AliasAnalysis = AliasAnalysis

type AAPass                 m   = PassMonad Namespace m
type AACtx              lab m a = (Enumerated lab, AATraversal m a)
type AATraversal            m a = (PassCtx m, AST.Traversal        AliasAnalysis (AAPass m) a a)
type AADefaultTraversal     m a = (PassCtx m, AST.DefaultTraversal AliasAnalysis (AAPass m) a a)

----------------------------------------------------------------------
-- Utils functions
----------------------------------------------------------------------

traverseM :: (AATraversal m a) => a -> AAPass m a
traverseM = AST.traverseM AliasAnalysis

defaultTraverseM :: (AADefaultTraversal m a) => a -> AAPass m a
defaultTraverseM = AST.defaultTraverseM AliasAnalysis

----------------------------------------------------------------------
-- Pass functions
----------------------------------------------------------------------

pass :: (Monoid s, AADefaultTraversal m a) => Pass s (a -> AAPass m AliasInfo)
pass = Pass "Alias analysis" 
            "Basic alias analysis that results in scope, alias, orphans and parent mapping information" 
            mempty aaUnit

aaUnit :: AADefaultTraversal m a => a -> AAPass m AliasInfo
aaUnit ast = defaultTraverseM ast *> (view Namespace.info <$> get)

aaMod :: AACtx lab m a => LModule lab a -> AAPass m (LModule lab a)
aaMod mod@(Label lab (Module path name body)) = withNewScope id continue
    where continue =  registerDecls body
                   *> defaultTraverseM mod
          id       = Enum.id lab

aaPat :: (PassCtx m, Enumerated lab) => LPat lab -> AAPass m (LPat lab)
aaPat p@(Label lab pat) = case pat of
    Pat.Var         name       -> regVarName id (Name.fromName name) *> continue
    _                          -> continue
    where id = Enum.id lab
          continue = defaultTraverseM p 

aaDecl :: AACtx lab m a => (LDecl lab a) -> AAPass m (LDecl lab a)
aaDecl d@(Label lab decl) = case decl of
    Decl.Function path name inputs output body -> withNewScope id $ defaultTraverseM d
    _                                          -> continue
    where id       = Enum.id lab
          continue = defaultTraverseM d

registerDecls :: AACtx lab m a => [LDecl lab a] -> AAPass m ()
registerDecls decls =  mapM_ registerHeaders  decls
                    *> mapM_ registerDataDecl decls

registerDataDecl :: AACtx lab m a => LDecl lab a -> AAPass m ()
registerDataDecl (Label lab decl) = case decl of
    Decl.Data     name _ cons defs   -> withNewScope id (registerDecls defs) *> pure ()
    _                                -> pure ()
    where id = Enum.id lab

registerHeaders :: AACtx lab m a => LDecl lab a -> AAPass m ()
registerHeaders (Label lab decl) = case decl of
    Decl.Function _ name inputs _ _  -> regVarName id (view MultiName.base name)
                                     <* withNewScope id (defaultTraverseM inputs)
    Decl.Data     name _ cons _      -> regTypeName id (Name.fromName name) 
                                     <* mapM_ registerCons cons
    _                                -> pure ()
    where id = Enum.id lab
          registerCons (Label lab (Decl.Cons name fields)) = regVarName (Enum.id lab) (Name.fromName name)


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance AACtx lab m a => AST.Traversal AliasAnalysis (AAPass m) (LModule lab a) (LModule lab a) where
    traverseM _ = aaMod

instance AACtx lab m a => AST.Traversal AliasAnalysis (AAPass m) (LDecl lab a) (LDecl lab a) where
    traverseM _ = aaDecl

instance (PassCtx m, Enumerated lab) => AST.Traversal AliasAnalysis (AAPass m) (LPat lab) (LPat lab) where
    traverseM _ = aaPat