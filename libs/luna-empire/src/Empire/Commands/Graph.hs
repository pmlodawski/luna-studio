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
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as Text

import qualified Empire.Data.Library     as Library
import qualified Empire.Data.Graph       as Graph
import           Empire.Data.Graph       (Graph)

import           Empire.API.Data.Project  (ProjectId)
import           Empire.API.Data.Library  (LibraryId)
import           Empire.API.Data.Port     (InPort(..), OutPort(..))
import           Empire.API.Data.PortRef  (InPortRef(..), OutPortRef(..))
import           Empire.API.Data.Node     (NodeId, Node(..))
import qualified Empire.API.Data.Node     as Node
import           Empire.API.Data.NodeMeta (NodeMeta)
import qualified Empire.API.Data.Graph    as API

import           Empire.Empire
import           Empire.Commands.Library      (withLibrary)
import qualified Empire.Commands.AST          as AST
import qualified Empire.Commands.GraphUtils   as GraphUtils
import qualified Empire.Commands.GraphBuilder as GraphBuilder

addNode :: ProjectId -> LibraryId -> Text -> NodeMeta -> Empire Node
addNode pid lid expr meta = withGraph pid lid $ do
    newNodeId <- gets Graph.nextNodeId
    refNode <- zoom Graph.ast $ AST.addNode ("node" ++ show newNodeId) (Text.unpack expr)
    zoom Graph.ast $ AST.writeMeta refNode (Just meta)
    Graph.nodeMapping . at newNodeId ?= refNode
    return $ Node.make newNodeId expr meta

updateNodeMeta :: ProjectId -> LibraryId -> NodeId -> NodeMeta -> Empire ()
updateNodeMeta pid lid nid meta = withGraph pid lid $ do
    ref <- GraphUtils.getASTPointer nid
    zoom Graph.ast $ AST.writeMeta ref $ Just meta

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
disconnect pid lid dstNodeId dstPort = withGraph pid lid $ do
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
    dstAst <- GraphUtils.getASTTarget nodeId
    newNodeRef <- zoom Graph.ast $ AST.runAstOp $ AST.unAcc dstAst
    GraphUtils.rewireNode nodeId newNodeRef

unApp :: NodeId -> Int -> Command Graph ()
unApp nodeId pos = do
    astNode <- GraphUtils.getASTTarget nodeId
    newNodeRef <- zoom Graph.ast $ AST.removeArg astNode pos
    GraphUtils.rewireNode nodeId newNodeRef

makeAcc :: NodeId -> NodeId -> Command Graph ()
makeAcc src dst = do
    srcAst <- GraphUtils.getASTVar src
    dstAst <- GraphUtils.getASTTarget dst
    newNodeRef <- zoom Graph.ast $ AST.runAstOp $ AST.makeAccessor srcAst dstAst
    GraphUtils.rewireNode dst newNodeRef

makeApp :: NodeId -> NodeId -> Int -> Command Graph ()
makeApp src dst pos = do
    srcAst <- GraphUtils.getASTVar src
    dstAst <- GraphUtils.getASTTarget dst
    newNodeRef <- zoom Graph.ast $ AST.applyFunction dstAst srcAst pos
    GraphUtils.rewireNode dst newNodeRef
