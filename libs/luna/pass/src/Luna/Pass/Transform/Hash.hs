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

module Luna.Pass.Transform.Hash where

import           Flowbox.Prelude              hiding (Traversal)
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
import qualified Luna.Syntax.Name.Pattern     as NamePat
import           Luna.Syntax.Name.Hash        (hash)

----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data Hash = Hash

type HPass                 m   = PassMonad () m
type HCtx              lab m a = (Enumerated lab, HTraversal m a)
type HTraversal            m a = (PassCtx m, AST.Traversal        Hash (HPass m) a a)
type HDefaultTraversal     m a = (PassCtx m, AST.DefaultTraversal Hash (HPass m) a a)


------------------------------------------------------------------------
---- Utils functions
------------------------------------------------------------------------

traverseM :: (HTraversal m a) => a -> HPass m a
traverseM = AST.traverseM Hash

defaultTraverseM :: (HDefaultTraversal m a) => a -> HPass m a
defaultTraverseM = AST.defaultTraverseM Hash


------------------------------------------------------------------------
---- Pass functions
------------------------------------------------------------------------

pass :: HDefaultTraversal m a => Pass () (a -> HPass m a)
pass = Pass "Hash" "Hashesh names removing all special characters" () defaultTraverseM

hashDecl :: (HCtx lab m a) => LDecl lab a -> HPass m (LDecl lab a)
hashDecl ast@(Label lab decl) = case decl of
    Decl.Func (Decl.FuncDecl path sig output body)
                                   -> return . Label lab . Decl.Func
                                    $ Decl.FuncDecl path (NamePat.mapSegments hashSegment sig) output body
    _                              -> continue
    where id       = Enum.id lab
          continue = defaultTraverseM ast

hashSegment = NamePat.mapSegmentBase hash


hashPat :: (MonadIO m, Applicative m, Enumerated lab, PassCtx m) => LPat lab -> HPass m (LPat lab)
hashPat ast@(Label lab pat) = case pat of
    Pat.Var name -> return . Label lab . Pat.Var $ fromText $ hash name
    _            -> continue
    where id       = Enum.id lab
          continue = defaultTraverseM ast

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------


instance (HCtx lab m a) => AST.Traversal Hash (HPass m) (LDecl lab a) (LDecl lab a) where
    traverseM _ = hashDecl

instance (MonadIO m, Applicative m, Enumerated lab, PassCtx m) => AST.Traversal Hash (HPass m) (LPat lab) (LPat lab) where
    traverseM _ = hashPat


