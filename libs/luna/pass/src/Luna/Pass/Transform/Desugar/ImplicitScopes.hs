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


module Luna.Pass.Transform.Desugar.ImplicitScopes where

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

data PassState = PassState { _astInfo    :: ASTInfo 
                           , _structInfo :: StructInfo 
                           } deriving (Show)


makeLenses ''PassState

data ImplScopes = ImplScopes

type ISPass                 m     = PassMonad PassState m
type ISCtx              lab m a   = (Monad m, Enumerated lab, ISTraversal m a, Num lab)
type ISTraversal            m a   = (PassCtx m, AST.Traversal        ImplScopes (ISPass m) a a)
type ISDefaultTraversal     m a   = (PassCtx m, AST.DefaultTraversal ImplScopes (ISPass m) a a)


------------------------------------------------------------------------
---- Utils functions
------------------------------------------------------------------------

traverseM :: (ISTraversal m a) => a -> ISPass m a
traverseM = AST.traverseM ImplScopes

defaultTraverseM :: (ISDefaultTraversal m a) => a -> ISPass m a
defaultTraverseM = AST.defaultTraverseM ImplScopes


------------------------------------------------------------------------
---- Pass functions
------------------------------------------------------------------------

pass :: ISDefaultTraversal m a => Pass PassState (ASTInfo -> StructInfo -> a -> ISPass m (a, ASTInfo))
pass = Pass "Implicit self" "Desugars AST by adding implicit self function parameters" undefined passRunner
-- TODO [wd]: ^----------------^-- add more detailed description, so the difference b/w this and ImplicitSelf would be clear

passRunner ai si ast = do
    put $ PassState ai si
    (,) <$> defaultTraverseM ast <*> (view astInfo <$> get)

exprScopes :: (MonadIO m, ISCtx lab m a) => LExpr lab a -> ISPass m (LExpr lab a)
exprScopes ast@(Label lab e) = case e of
    Expr.Var (Expr.Variable name v) -> fmake ast id
    --Expr.Curry v@(Label lab' (Expr.Var {}) ->  fmake v
    _                               -> continue
    where continue = defaultTraverseM ast

fmake ast@(Label lab e) f = case e of
    Expr.Var (Expr.Variable name v) -> do
        si <- view structInfo <$> get
        let parentMap = view StructInfo.parent si
            aliasMap  = view StructInfo.alias si
            pid       = view (at id) parentMap
            tgt       = fmap (view StructInfo.target) $ view (at id) aliasMap
            tgtPid    = join $ fmap (\tid -> view (at tid) parentMap) tgt
        liftIO . putStrLn $ "MINE" ++ show pid ++ " " ++ show tgtPid ++ "\nNow AST:\n" ++ show name
        case pid == tgtPid of
            True  -> return ast
            False -> return $ Label (-888) $ Expr.Accessor (convert name) (f $ Label lab $ Expr.Var $ Expr.Variable "self" v)
    where id = Enum.id lab
----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance ISCtx lab m a => AST.Traversal ImplScopes (ISPass m) (LExpr lab a) (LExpr lab a) where
    traverseM _ = exprScopes

