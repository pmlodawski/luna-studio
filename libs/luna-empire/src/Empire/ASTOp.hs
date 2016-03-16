{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module Empire.ASTOp where

import           Prologue                               hiding (Num, Cons)

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
import           Luna.Syntax.Model.Network.Builder.Term (TermBuilder, NetLayers, NetworkBuilderT, runNetworkBuilderT)
import           Luna.Syntax.Model.Network.Term         (Raw)
import           Luna.Syntax.AST.Term                   (Acc, App, Blank, Match, Var, Cons, Unify)
import           Luna.Syntax.Model.Layer                ((:<:))
import qualified Luna.Syntax.AST.Term.Lit               as Lit
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
               , TermBuilder Blank      m NodeRef
               , TermBuilder Lit.Number m NodeRef
               , TermBuilder Lit.String m NodeRef
               , TermBuilder Acc        m NodeRef
               , TermBuilder App        m NodeRef
               , TermBuilder Match      m NodeRef
               , TermBuilder Var        m NodeRef
               , TermBuilder Unify      m NodeRef
               , TermBuilder Cons       m NodeRef
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
