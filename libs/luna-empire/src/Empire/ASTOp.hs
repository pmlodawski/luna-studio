{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module Empire.ASTOp where

import           Prologue

import           Control.Monad.Error                    (ErrorT, MonadError, runErrorT)
import           Empire.Data.AST                        (AST, ASTEdge, ASTNode, NodeRef, EdgeRef)
import           Empire.Empire                          (Command, Error, empire)
import           Data.Construction                      (Destructor)
import           Luna.Syntax.Model.Network.Builder.Self (MonadSelfBuilder)
import           Luna.Syntax.Model.Network.Builder.Type (MonadTypeBuilder)
import           Luna.Syntax.Model.Graph.Builder        (MonadBuilder)
import           Luna.Syntax.Model.Graph.Edge           (Connectible)
import           Luna.Syntax.Model.Network.Builder.Term    (ElemBuilder, NetLayers, NetworkBuilderT, runNetworkBuilderT)
import           Luna.Syntax.Model.Network.Term    (Raw)
import           Luna.Syntax.AST.Term    (Acc, App, Blank, Unify, Var, Str)
import           Luna.Syntax.Model.Layer ((:<))


type ASTOp m = ( MonadIO m
               , MonadFix m
               , MonadError Error m
               , Destructor m NodeRef
               , MonadBuilder (NetLayers :< Raw) ASTEdge m
               , MonadSelfBuilder ASTNode m
               , MonadTypeBuilder ASTNode m
               , ElemBuilder Blank             m NodeRef
               , ElemBuilder (Acc Str EdgeRef) m NodeRef
               , ElemBuilder (App EdgeRef)     m NodeRef
               , ElemBuilder (Unify EdgeRef)   m NodeRef
               , ElemBuilder (Var Str)         m NodeRef
               , Connectible NodeRef NodeRef m
               )

runGraph :: NetworkBuilderT AST m IO => ErrorT Error m a -> AST -> IO (Either Error a, AST)
runGraph cmd g = runNetworkBuilderT g
               $ runErrorT cmd

runASTOp :: NetworkBuilderT AST m IO => ErrorT Error m a -> Command AST a
runASTOp = empire . runGraph
