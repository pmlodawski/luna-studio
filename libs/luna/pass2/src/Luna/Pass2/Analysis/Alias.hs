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
import           Luna.Pass2.Pass              (Pass, PassMonad)
import qualified Luna.Pass2.Pass              as Pass

import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)

import           Luna.Data.AliasInfo          (AliasInfo)

import qualified Luna.Data.Namespace.State    as State 
import           Luna.Data.Namespace.State    (regVarName, regTypeName, withNewScope)


----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data AliasAnalysis = AliasAnalysis

type VAError           = String
type VAPass m          = Pass VAError Namespace m
type VAMonoCtx lab m a = (PassMonad m, Enumerated lab, MonoTraversal m a)
type MonoTraversal m a = AST.MonoTraversal AliasAnalysis (VAPass m) a

----------------------------------------------------------------------
-- Utils functions
----------------------------------------------------------------------

traverseM :: (AST.MonoTraversal AliasAnalysis m a, PassMonad m) => a -> m a
traverseM = AST.traverseM AliasAnalysis

defaultTraverseM :: (AST.DefaultTraversal AliasAnalysis m a a, PassMonad m) => a -> m a
defaultTraverseM = AST.defaultMonoTraverseM AliasAnalysis

----------------------------------------------------------------------
-- Pass functions
----------------------------------------------------------------------

run :: VAMonoCtx lab m a => Unit (LModule lab a) -> Pass.Result m VAError AliasInfo
run = (Pass.run_ (Pass.Info "Alias") mempty) . aaUnit


aaUnit :: VAMonoCtx lab m (LModule lab a) => Unit (LModule lab a) -> VAPass m AliasInfo
aaUnit (ast :: Unit (LModule a e)) = traverseM ast *> (view Namespace.info <$> get)



aaMod :: VAMonoCtx lab m a => LModule lab a -> VAPass m (LModule lab a)
aaMod mod@(Label lab (Module path name body)) = withNewScope id continue
    where continue =  registerDecls body
                   *> defaultTraverseM mod
          id       = Enum.id lab

aaPat :: (MonadIO m, Functor m, Enumerated lab) => LPat lab -> VAPass m (LPat lab)
aaPat p@(Label lab pat) = case pat of
    Pat.Var         name       -> regVarName id (Name.fromName name) *> continue
    _                          -> continue
    where id = Enum.id lab
          continue = defaultTraverseM p 

aaDecl :: VAMonoCtx lab m a => (LDecl lab a) -> VAPass m (LDecl lab a)
aaDecl d@(Label lab decl) = case decl of
    Decl.Function path name inputs output body -> withNewScope id $ defaultTraverseM d
    _                                          -> continue
    where id       = Enum.id lab
          continue = defaultTraverseM d

registerDecls :: VAMonoCtx lab m a => ,

[LDecl lab a] -> VAPass m ()
registerDecls decls =  mapM_ registerHeaders  decls
                    *> mapM_ registerDataDecl decls

registerDataDecl :: VAMonoCtx lab m a => LDecl lab a -> VAPass m ()
registerDataDecl (Label lab decl) = case decl of
    Decl.Data     name _ cons defs   -> withNewScope id (registerDecls defs) *> pure ()
    _                                -> pure ()
    where id = Enum.id lab

registerHeaders :: VAMonoCtx lab m a => LDecl lab a -> VAPass m ()
registerHeaders (Label lab decl) = case decl of
    Decl.Function _ name inputs _ _  -> regVarName id (view MultiName.base name)
                                     <* withNewScope id (traverseM inputs)
    Decl.Data     name _ cons _      -> regTypeName id (Name.fromName name) 
                                     <* mapM_ registerCons cons
    _                                -> pure ()
    where id = Enum.id lab
          registerCons (Label lab (Decl.Cons name fields)) = regVarName (Enum.id lab) (Name.fromName name)


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance VAMonoCtx lab m a => AST.Traversal AliasAnalysis (VAPass m) (LModule lab a) (LModule lab a) where
    traverseM _ = aaMod

instance VAMonoCtx lab m a => AST.Traversal AliasAnalysis (VAPass m) (LDecl lab a) (LDecl lab a) where
    traverseM _ = aaDecl

instance (MonadIO m, Functor m, Enumerated lab) 
      => AST.Traversal AliasAnalysis (VAPass m) (LPat lab) (LPat lab) where
    traverseM _ = aaPat