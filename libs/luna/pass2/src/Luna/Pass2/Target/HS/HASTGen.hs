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

module Luna.Pass2.Target.HS.HASTGen where

import           Flowbox.Prelude              hiding (Traversal)
import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join, mapM, State)
import qualified Luna.ASTNew.Traversals       as AST
--import qualified Luna.ASTNew.Enum             as Enum
import           Luna.ASTNew.Enum             (Enumerated, IDTag(IDTag))
--import qualified Luna.ASTNew.Decl             as Decl
--import           Luna.ASTNew.Decl             (LDecl, Field(Field))
import qualified Luna.ASTNew.Module           as Module
import           Luna.ASTNew.Module           (Module(Module), LModule)
import           Luna.ASTNew.Unit             (Unit(Unit))
import qualified Luna.ASTNew.Label            as Label
import           Luna.ASTNew.Label            (Label(Label))
--import qualified Luna.ASTNew.Type             as Type
--import           Luna.ASTNew.Type             (Type)
--import qualified Luna.ASTNew.Pat              as Pat
--import           Luna.ASTNew.Pat              (LPat, Pat)
--import           Luna.ASTNew.Expr             (LExpr, Expr)
--import qualified Luna.ASTNew.Lit              as Lit
--import qualified Luna.ASTNew.Native           as Native
import qualified Luna.ASTNew.Name             as Name
--import           Luna.ASTNew.Name             (TName(TName), TVName(TVName))
import qualified Luna.Data.HAST.Expr                         as HExpr
import qualified Luna.Data.HAST.Lit                          as HLit
import qualified Luna.Data.HAST.Module                       as HModule
import qualified Luna.Data.HAST.Extension                    as HExtension
import qualified Luna.Data.HAST.Comment                      as HComment

import           Luna.Pass                    (Pass(Pass), PassMonad)
import qualified Luna.Pass                    as Pass

import qualified Luna.Data.Namespace          as Namespace
import           Luna.Data.Namespace          (Namespace)

import           Luna.Data.ASTInfo            (ASTInfo, genID)

import qualified Luna.Parser.Parser           as Parser
import           Luna.ASTNew.Name.Pattern2    (NamePat(NamePat), Segment(Segment), Arg(Arg))
import qualified Luna.ASTNew.Name.Pattern2    as NamePat

import qualified Luna.Pass2.Target.HS.HASTGen.State as State



----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data HASTGen = HASTGen

type PassResult           m   = PassMonad State.GenState m
type PassCtx          lab m a = (Enumerated lab, Traversal m a)
type Traversal            m a = (Pass.PassCtx m, AST.Traversal        HASTGen (PassResult m) a a)
type DefaultTraversal     m a = (Pass.PassCtx m, AST.DefaultTraversal HASTGen (PassResult m) a a)


type HExpr = HExpr.Expr

------------------------------------------------------------------------
---- Utils functions
------------------------------------------------------------------------

traverseM :: (Traversal m a) => a -> PassResult m a
traverseM = AST.traverseM HASTGen

defaultTraverseM :: (DefaultTraversal m a) => a -> PassResult m a
defaultTraverseM = AST.defaultTraverseM HASTGen


------------------------------------------------------------------------
---- Pass functions
------------------------------------------------------------------------

pass :: Monad m => Pass State.GenState (Unit (LModule a e) -> PassResult m HExpr)
pass = Pass "HASTGen" "Haskell AST generator" def genUnit

genUnit (Unit m) = genModule m


genModule :: Monad m => LModule a e -> PassResult m HExpr
genModule (Label lab (Module path name body)) = do
    let mod     = HModule.addImport ["Luna", "Target", "HS"]
                $ HModule.addExt HExtension.DataKinds
                $ HModule.addExt HExtension.DeriveDataTypeable
                $ HModule.addExt HExtension.DeriveGeneric
                $ HModule.addExt HExtension.DysfunctionalDependencies
                $ HModule.addExt HExtension.NoMonomorphismRestriction
                $ HModule.addExt HExtension.FlexibleContexts
                $ HModule.addExt HExtension.FlexibleInstances
                $ HModule.addExt HExtension.GADTs
                $ HModule.addExt HExtension.RebindableSyntax
                $ HModule.addExt HExtension.TemplateHaskell
                $ HModule.addExt HExtension.UndecidableInstances
                $ HModule.addExt HExtension.ViewPatterns
                $ HModule.mk (["TEST"])
                -- $ HModule.mk (path ++ [name])
        --params  = view LType.params cls
        --modCon  = LExpr.ConD 0 name fields
        --modConName = Naming.modCon name
    
    State.setModule mod
    
    --State.addComment $ HExpr.Comment $ HComment.H1 $ "Data types"
    --genCon' cls modCon stdDerivings
    --State.addComment $ HExpr.Comment $ HComment.H5 $ "Other data types"
    --mapM_ genExpr classes

    --State.setCls    cls

    --State.addComment $ HExpr.Comment $ HComment.H1 $ "Type aliases"
    --mapM_ (genExpr >=> State.addTypeAlias) typeAliases

    --State.addComment $ HExpr.Comment $ HComment.H1 $ "Type defs"
    --mapM_ (genExpr >=> State.addTypeDef)   typeDefs

    ----genCon' cls modCon stdDerivings
    ---- DataType
    ----consTH
    
    ----State.addTHExpression $ thGenerateAccessors name
    ----State.addTHExpression $ thRegisterAccessors name
    ----State.addTHExpression $ thInstsAccessors name

    --State.addComment $ HExpr.Comment $ HComment.H1 $ "Module methods"
    --mapM_ genExpr methods
    
    --mapM_ (genExpr >=> State.addImport) imports
    --when (name == "Main") $ do
    --    State.addComment $ HExpr.Comment $ HComment.H1 $ "Main module wrappers"
    --    State.addFunction $ mainf modConName
    State.getModule


--hashDecl :: (PassCtx lab m a) => LDecl lab a -> Pass m (LDecl lab a)
--hashDecl ast@(Label lab decl) = case decl of
--    Decl.Function path sig output body -> return . Label lab
--                                        $ Decl.Function path (NamePat.mapSegments hashSegment sig) output body
--    _                                  -> continue
--    where id       = Enum.id lab
--          continue = defaultTraverseM ast

--hashSegment = NamePat.mapSegmentBase hash


--hashPat :: (MonadIO m, Applicative m, Enumerated lab) => LPat lab -> Pass m (LPat lab)
--hashPat ast@(Label lab pat) = case pat of
--    Pat.Var name -> return . Label lab . Pat.Var $ fromString $ hash name
--    _            -> continue
--    where id       = Enum.id lab
--          continue = defaultTraverseM ast

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------


--instance (PassCtx lab m a) => AST.Traversal Pass (Pass m) (LDecl lab a) (LDecl lab a) where
--    traverseM _ = hashDecl

--instance (MonadIO m, Applicative m, Enumerated lab) => AST.Traversal Pass (Pass m) (LPat lab) (LPat lab) where
--    traverseM _ = hashPat


