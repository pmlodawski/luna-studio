{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module Empire.ASTOp where

import           Prologue                               hiding (Num, Cons, Curry)

import           Control.Monad.Error                    (ErrorT, MonadError, runErrorT)
import           Data.Proxy
import           Data.Graph                             (Node)
import           Data.Graph.Model.Events                (ELEMENT (..))
import           Empire.Data.AST                        (AST, ASTEdge, ASTNode, NodeRef, EdgeRef)
import           Empire.API.Data.NodeMeta               (NodeMeta)
import           Empire.Empire                          (Command, Error, empire)
import           Data.Construction                      (Destructor, Unregister)
import           Data.Graph.Builder                     (MonadBuilder)
import           Data.Graph.Builders                    (Connectible)
import           Luna.Syntax.Model.Network.Builder.Term (TermBuilder_OLD, NetLayers, NetworkBuilderT, runNetworkBuilderT)
import           Luna.Syntax.Model.Network.Builder.Node (TermNode)
import           Luna.Syntax.Model.Network.Term         (Raw)
import           Old.Luna.Syntax.Term.Class             (Acc, App, Blank, Match, Var, Cons, Unify, Curry, Lam)
import           Luna.Syntax.Model.Layer                ((:<:))
import qualified Old.Luna.Syntax.Term.Expr.Lit               as Lit
import           Type.Inference
import           Luna.Syntax.Model.Network.Builder       (Reconnectible)
import qualified Luna.Syntax.Model.Network.Builder.Class as Builder
import           Luna.Syntax.Model.Text.Location         (MonadLocation, LocationT)
import qualified Luna.Syntax.Model.Text.Location         as Location

type ASTOp m = ( MonadIO m
               , MonadFix m
               , MonadError Error m
               , Destructor m NodeRef
               , Unregister m EdgeRef
               , MonadBuilder AST m
               , MonadLocation m
               , TermBuilder_OLD Blank      m NodeRef
               , TermBuilder_OLD Lit.Number m NodeRef
               , TermBuilder_OLD Lit.String m NodeRef
               , TermBuilder_OLD Acc        m NodeRef
               , TermBuilder_OLD App        m NodeRef
               , TermBuilder_OLD Match      m NodeRef
               , TermBuilder_OLD Var        m NodeRef
               , TermBuilder_OLD Unify      m NodeRef
               , TermBuilder_OLD Cons       m NodeRef
               , TermBuilder_OLD Curry      m NodeRef
               , TermBuilder_OLD Lam        m NodeRef
               , Connectible NodeRef NodeRef m
               )

runBuilder :: NetworkBuilderT AST m (KnownTypeT ELEMENT NodeRef n) => Builder.NetworkBuilderT m a -> AST -> n (a, AST)
runBuilder cmd ast = runInferenceT ELEMENT (Proxy :: Proxy NodeRef)
                   $ runNetworkBuilderT ast
                   $ Builder.runNetworkBuilderT cmd

runGraph :: (Monad n, NetworkBuilderT AST m (LocationT (KnownTypeT ELEMENT NodeRef n))) => Builder.NetworkBuilderT (ErrorT Error m) a -> AST -> n (Either Error a, AST)
runGraph cmd g = runInferenceT ELEMENT (Proxy :: Proxy NodeRef)
               $ flip Location.evalT Nothing
               $ runNetworkBuilderT g
               $ runErrorT
               $ Builder.runNetworkBuilderT
               $ cmd

runASTOp :: NetworkBuilderT AST m (LocationT (KnownTypeT ELEMENT NodeRef IO)) => Builder.NetworkBuilderT (ErrorT Error m) a -> Command AST a
runASTOp = empire . const . runGraph
