---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}


module Luna.Pass.Transform.Desugar.ImplicitCalls where

import Data.Typeable
import GHC.TypeLits

import           Flowbox.Control.Monad.State hiding (State, join, mapM, mapM_)
import           Flowbox.Prelude             hiding (Traversal)
import           Luna.Data.ASTInfo           (ASTInfo)
import qualified Luna.Data.ASTInfo           as ASTInfo
import           Luna.Pass                   (Pass (Pass), PassCtx, PassMonad)
import           Luna.Syntax.Enum            (Enumerated)
import qualified Luna.Syntax.Enum            as Enum
import           Luna.Syntax.Expr            (LExpr)
import qualified Luna.Syntax.Expr            as Expr
import           Luna.Syntax.Label           (Label (Label))
import           Luna.Syntax.Name.Pattern    (NamePat (NamePat), Segment (Segment))
import qualified Luna.Syntax.Traversals      as AST

----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data ImplCalls = ImplCalls
data ImplCallsOmit (n :: Nat) = ImplCallsOmit

implCallsOmit :: Proxy n -> ImplCallsOmit n
implCallsOmit _ = ImplCallsOmit

type ISPass                 m     = PassMonad ASTInfo m
type ISCtx              lab m n a = (Monad m, Enumerated lab, ISTraversal m a, ISTraversalOmit m n a, Num lab)
type ISCtx2             lab m a   = (Monad m, Enumerated lab, ISTraversal m a, Num lab)
type ISTraversal            m a   = (PassCtx m, AST.Traversal        ImplCalls (ISPass m) a a)
type ISTraversalOmit        m n a = (PassCtx m, AST.Traversal        (ImplCallsOmit n) (ISPass m) a a)
type ISDefaultTraversal     m a   = (PassCtx m, AST.DefaultTraversal ImplCalls (ISPass m) a a)
type ISDefaultTraversalOmit m n a = (PassCtx m, AST.DefaultTraversal (ImplCallsOmit n) (ISPass m) a a)


------------------------------------------------------------------------
---- Utils functions
------------------------------------------------------------------------

traverseM :: (ISTraversal m a) => a -> ISPass m a
traverseM = AST.traverseM ImplCalls

defaultTraverseM :: (ISDefaultTraversal m a) => a -> ISPass m a
defaultTraverseM = AST.defaultTraverseM ImplCalls

defaultTraverseOmitM :: (ISDefaultTraversalOmit m n a) => Proxy n -> a -> ISPass m a
defaultTraverseOmitM p = AST.defaultTraverseM (implCallsOmit p)


------------------------------------------------------------------------
---- Pass functions
------------------------------------------------------------------------

pass :: ISDefaultTraversal m a => Pass ASTInfo (ASTInfo -> a -> ISPass m (a, ASTInfo))
pass = Pass "Implicit calls" "Desugars AST by adding implicit calls" undefined passRunner

passRunner ai ast = do
    put ai
    (,) <$> defaultTraverseM ast <*> get

exprScopes :: (ISCtx lab m 1 a) => LExpr lab a -> ISPass m (LExpr lab a)
exprScopes ast@(Label lab e) = case e of
    Expr.Cons     {} -> Label <$> tag <*> (Expr.app <$> continue <*> pure [])
    Expr.Accessor {} -> Label <$> tag <*> (Expr.app <$> continue <*> pure [])
    Expr.Curry (Label lab' acc@(Expr.Accessor {})) -> Label lab . Expr.Curry <$> (Label lab' <$> defaultTraverseOmitM (Proxy::Proxy 1) acc)
    Expr.App (NamePat pfx (Segment base args) segs) ->
        (Label lab . Expr.App) <$> (NamePat <$> defaultTraverseM pfx
                                            <*> (Segment <$> procSeg base
                                                         <*> defaultTraverseM allArgs)
                                            <*> pure [])
        where getSegArgs (Segment _ args) = args
              allArgs = args ++ concat (fmap getSegArgs segs)
    _                -> continue
    where
        continue = defaultTraverseM ast
        id       = Enum.id lab
        tag      = fromIntegral <$> ASTInfo.genID

procSeg expr@(Label lab e) = case e of
    Expr.Accessor {} -> defaultTraverseM expr
    _                -> defaultTraverseOmitM (Proxy::Proxy 1) expr

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance (ISCtx lab m 1 a) => AST.Traversal ImplCalls (ISPass m) (LExpr lab a) (LExpr lab a) where
    traverseM _ = exprScopes


instance (ISCtx lab m n a, ISTraversalOmit m (n-1) a, ISTraversalOmit m (n-1) (LExpr lab a)) => AST.Traversal (ImplCallsOmit n) (ISPass m) (LExpr lab a) (LExpr lab a) where
    traverseM _ = defaultTraverseOmitM (Proxy :: Proxy (n-1))


instance (ISCtx lab m 1 a) => AST.Traversal (ImplCallsOmit 1) (ISPass m) (LExpr lab a) (LExpr lab a) where
    traverseM _ = defaultTraverseM
