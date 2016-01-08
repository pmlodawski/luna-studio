module Empire.Commands.Graph
    ( addNode
    , removeNode
    , connect
    , disconnect
    , getCode
    , getGraph
    ) where

import           Prologue
import           Control.Monad.State
import           Control.Monad.Error     (throwError)
import qualified Data.IntMap             as Map

import qualified Empire.Data.Library     as Library
import qualified Empire.Data.Graph       as Graph
import           Empire.Data.Graph       (Graph)

import           Empire.API.Data.Project (ProjectId)
import           Empire.API.Data.Library (LibraryId)
import           Empire.API.Data.Port    (InPort(..), OutPort(..))
import           Empire.API.Data.PortRef (InPortRef(..), OutPortRef(..))
import           Empire.API.Data.Node    (NodeId)
import qualified Empire.API.Data.Graph   as API

import           Empire.Empire
import           Empire.Commands.Library      (withLibrary)
import qualified Empire.Commands.AST          as AST
import qualified Empire.Commands.GraphUtils   as GraphUtils
import qualified Empire.Commands.GraphBuilder as GraphBuilder
import           Luna.Syntax.Repr.Graph       (Ref, Node)

-- TODO: change NodeId to Node
addNode :: ProjectId -> LibraryId -> String -> Empire NodeId
addNode pid lid expr = withGraph pid lid $ do
    newNodeId <- gets Graph.nextNodeId
    refNode <- zoom Graph.ast $ AST.addNode ("node" ++ show newNodeId) expr
    Graph.nodeMapping . at newNodeId ?= refNode
    return newNodeId

removeNode :: ProjectId -> LibraryId -> NodeId -> Empire ()
removeNode pid lid nodeId = withGraph pid lid $ do
    astNode <- use (Graph.nodeMapping . at nodeId) <?!> "Node does not exist"
    Graph.nodeMapping %= Map.delete nodeId
    zoom Graph.ast $ AST.removeGraphNode astNode

connect :: ProjectId -> LibraryId -> NodeId -> OutPort -> NodeId -> InPort -> Empire ()
connect pid lid srcNodeId All dstNodeId dstPort = withGraph pid lid $ do
    case dstPort of
        Self    -> makeAcc srcNodeId dstNodeId
        Arg num -> makeApp srcNodeId dstNodeId num
connect pid lid srcNodeId _ dstNodeId dstPort = throwError "Source port should be All"

disconnect :: ProjectId -> LibraryId -> NodeId -> InPort -> Empire ()
disconnect pid lid dstNodeId dstPort = disconnect' pid lid undefined undefined dstNodeId dstPort

disconnect' :: ProjectId -> LibraryId -> NodeId -> OutPort -> NodeId -> InPort -> Empire ()
disconnect' pid lid _ _ dstNodeId dstPort = withGraph pid lid $ do
    case dstPort of
        Self    -> unAcc dstNodeId
        Arg num -> unApp dstNodeId num

getCode :: ProjectId -> LibraryId -> Empire String
getCode pid lid = withGraph pid lid $ do
    allNodes <- uses Graph.nodeMapping Map.keys
    lines <- sequence $ printNodeLine <$> allNodes
    return $ intercalate "\n" lines

getGraph :: ProjectId -> LibraryId -> Empire API.Graph
getGraph pid lid = withGraph pid lid GraphBuilder.buildGraph

-- internal

printNodeLine :: NodeId -> Command Graph String
printNodeLine nid = GraphUtils.getASTPointer nid >>= (zoom Graph.ast . AST.runAstOp . AST.prettyPrint)

withGraph :: ProjectId -> LibraryId -> Command Graph a -> Empire a
withGraph pid lid = withLibrary pid lid . zoom Library.body

unAcc :: NodeId -> Command Graph ()
unAcc nodeId = do
    dstAst <- use (Graph.nodeMapping . at nodeId) <?!> "Node does not exist"
    nameNode <- zoom Graph.ast $ AST.getNameNode dstAst
    {-zoom Graph.ast $ AST.removeNode dstAst-}
    newNodeRef <- zoom Graph.ast $ AST.makeVar nameNode
    Graph.nodeMapping . at nodeId ?= newNodeRef

unApp :: NodeId -> Int -> Command Graph ()
unApp nodeId pos = do
    astNode <- use (Graph.nodeMapping . at nodeId) <?!> "Node does not exist"
    newNodeRef <- zoom Graph.ast $ AST.removeArg astNode pos
    Graph.nodeMapping . at nodeId ?= newNodeRef

rewireNode :: NodeId -> Ref Node -> Command Graph ()
rewireNode nodeId newTarget = do
    unifyNode <- GraphUtils.getASTPointer nodeId
    zoom Graph.ast $ AST.replaceTargetNode unifyNode newTarget

makeAcc :: NodeId -> NodeId -> Command Graph ()
makeAcc src dst = do
    srcAst <- GraphUtils.getASTVar src
    dstAst <- GraphUtils.getASTTarget dst
    newNodeRef <- zoom Graph.ast $ AST.runAstOp $ AST.makeAccessor srcAst dstAst
    rewireNode dst newNodeRef

makeApp :: NodeId -> NodeId -> Int -> Command Graph ()
makeApp src dst pos = do
    srcAst <- GraphUtils.getASTVar src
    dstAst <- GraphUtils.getASTTarget dst
    newNodeRef <- zoom Graph.ast $ AST.applyFunction dstAst srcAst pos
    rewireNode dst newNodeRef
