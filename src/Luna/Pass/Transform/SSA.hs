---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Pass.Transform.SSA where

import           Flowbox.Prelude             hiding (Traversal)
import           Luna.Pass                   (Pass (Pass), PassCtx, PassMonad)
import           Luna.Syntax.Enum            (Enumerated)
import qualified Luna.Syntax.Traversals      as AST
--import           Flowbox.Control.Monad.State hiding (State, join, mapM, mapM_)
--import qualified Luna.Syntax.Enum             as Enum
--import qualified Luna.Syntax.Decl             as Decl
--import           Luna.Syntax.Decl             (LDecl, Field(Field))
--import qualified Luna.Syntax.Module           as Module
--import           Luna.Syntax.Module           (Module(Module), LModule)
--import           Luna.Syntax.Unit             (Unit(Unit))
--import qualified Luna.Syntax.Label            as Label
--import           Luna.Syntax.Label            (Label(Label))
--import qualified Luna.Syntax.Type             as Type
--import           Luna.Syntax.Type             (Type)
--import qualified Luna.Syntax.Pat              as Pat
--import           Luna.Syntax.Pat              (LPat, Pat)
--import           Luna.Syntax.Expr             (LExpr, Expr)
--import qualified Luna.Syntax.Lit              as Lit
--import qualified Luna.Syntax.Native           as Native
--import qualified Luna.Syntax.Name             as Name
--import           Luna.Syntax.Name             (TName(TName), TVName(TVName))
--import qualified Luna.Pass                    as Pass
--import qualified Luna.Data.Namespace          as Namespace
--import           Luna.Data.Namespace          (Namespace)
--import           Luna.Data.ASTInfo            (ASTInfo, genID)
--import qualified Luna.Data.Namespace.State    as State
--import qualified Luna.Parser.Parser           as Parser
--import qualified Luna.Parser.State            as ParserState
--import           Luna.Syntax.Name.Pattern     (NamePat(NamePat), Segment(Segment))
--import qualified Luna.Syntax.Name.Pattern     as NamePat

----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data SSA = SSA

type SSAPass                 m   = PassMonad () m
type SSACtx              lab m a = (Enumerated lab, SSATraversal m a)
type SSATraversal            m a = (PassCtx m, AST.Traversal        SSA (SSAPass m) a a)
type SSADefaultTraversal     m a = (PassCtx m, AST.DefaultTraversal SSA (SSAPass m) a a)


------------------------------------------------------------------------
---- Utils functions
------------------------------------------------------------------------

traverseM :: (SSATraversal m a) => a -> SSAPass m a
traverseM = AST.traverseM SSA

defaultTraverseM :: (SSADefaultTraversal m a) => a -> SSAPass m a
defaultTraverseM = AST.defaultTraverseM SSA


------------------------------------------------------------------------
---- Pass functions
------------------------------------------------------------------------

pass :: SSADefaultTraversal m a => Pass () (a -> SSAPass m a)
pass = Pass "SSA" "Single Static Assignment pass" () defaultTraverseM

--hashDecl :: (SSACtx lab m a) => LDecl lab a -> SSAPass m (LDecl lab a)
--hashDecl ast@(Label lab decl) = case decl of
--    Decl.Function path sig output body -> return . Label lab
--                                        $ Decl.Function path (NamePat.mapSegments hashSegment sig) output body
--    _                                  -> continue
--    where id       = Enum.id lab
--          continue = defaultTraverseM ast

--hashSegment = NamePat.mapSegmentBase hash


--hashPat :: (MonadIO m, Applicative m, Enumerated lab) => LPat lab -> SSAPass m (LPat lab)
--hashPat ast@(Label lab pat) = case pat of
--    Pat.Var name -> return . Label lab . Pat.Var $ fromString $ hash name
--    _            -> continue
--    where id       = Enum.id lab
--          continue = defaultTraverseM ast

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------


--instance (SSACtx lab m a) => AST.Traversal SSA (SSAPass m) (LDecl lab a) (LDecl lab a) where
--    traverseM _ = hashDecl

--instance (MonadIO m, Applicative m, Enumerated lab) => AST.Traversal SSA (SSAPass m) (LPat lab) (LPat lab) where
--    traverseM _ = hashPat


