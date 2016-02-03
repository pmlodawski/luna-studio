{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}

module Empire.ASTOp where

import           Prologue
import           Control.Monad.Error             (ErrorT, runErrorT, MonadError)
import           Empire.Data.AST                 (AST, ASTNode, ASTEdge)
import           Empire.Empire                   (Command, Error, empire)
import           Luna.Syntax.Model.Graph.Term    (NetworkBuilderT, runNetworkBuilderT, NetLayers, Raw)
import           Luna.Syntax.Model.Builder.Self        (MonadSelfBuilder)
import           Luna.Syntax.Model.Builder.Type        (MonadTypeBuilder)
import           Luna.Syntax.Model.Graph.Builder       (MonadBuilder)
import           Luna.Syntax.Model.Layer.Class         ((:<))

type ASTOp m = ( MonadIO m
               ,  MonadError Error m
               ,  MonadBuilder (NetLayers :< Raw) ASTEdge m
               ,  MonadSelfBuilder ASTNode m
               ,  MonadTypeBuilder ASTNode m
               )

runGraph :: NetworkBuilderT AST m IO => ErrorT Error m a -> AST -> IO (Either Error a, AST)
runGraph cmd g = runNetworkBuilderT g
               $ runErrorT cmd

runASTOp :: NetworkBuilderT AST m IO => ErrorT Error m a -> Command AST a
runASTOp = empire . runGraph
