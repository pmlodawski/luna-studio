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
import           Empire.ASTOps.Remove    (safeRemove)

getASTPointer :: NodeId -> Command Graph NodeRef
getASTPointer nodeId = use (Graph.nodeMapping . at nodeId) <?!> "Node does not exist"

getASTTarget :: NodeId -> Command Graph NodeRef
getASTTarget nodeId = do
    matchNode <- getASTPointer nodeId
    zoom Graph.ast $ AST.getTargetNode matchNode

getASTVar :: NodeId -> Command Graph NodeRef
getASTVar nodeId = do
    matchNode <- getASTPointer nodeId
    zoom Graph.ast $ AST.getVarNode matchNode

rewireNode :: NodeId -> NodeRef -> Command Graph ()
rewireNode nodeId newTarget = do
    matchNode <- getASTPointer nodeId
    oldTarget <- getASTTarget  nodeId
    zoom Graph.ast $ do
        AST.replaceTargetNode matchNode newTarget
        AST.removeSubtree oldTarget
