module Empire.Commands.GraphUtils where

import           Prologue

import           Control.Monad.Error     (throwError)
import           Control.Monad.State
import           Empire.Empire

import           Empire.Data.AST         (NodeRef)
import           Empire.Data.Graph       (Graph)
import qualified Empire.Data.Graph       as Graph
import           Empire.API.Data.Node    (NodeId)
import qualified Empire.Commands.AST     as AST
import qualified Empire.ASTOp            as AST

getASTPointer :: NodeId -> Command Graph NodeRef
getASTPointer nodeId = use (Graph.nodeMapping . at nodeId) <?!> "Node does not exist"

getASTTarget :: NodeId -> Command Graph NodeRef
getASTTarget nodeId = do
    unifyNode <- getASTPointer nodeId
    zoom Graph.ast $ AST.getTargetNode unifyNode

getASTVar :: NodeId -> Command Graph NodeRef
getASTVar nodeId = do
    unifyNode <- getASTPointer nodeId
    zoom Graph.ast $ AST.getVarNode unifyNode

rewireNode :: NodeId -> NodeRef -> Command Graph ()
rewireNode nodeId newTarget = do
    unifyNode <- getASTPointer nodeId
    zoom Graph.ast $ AST.replaceTargetNode unifyNode newTarget
