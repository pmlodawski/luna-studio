module Empire.Commands.Graph
    ( addNode
    , removeNode
    , updateNodeMeta
    , connect
    , disconnect
    , getCode
    , getGraph
    , runGraph
    ) where

import           Prologue
import           Control.Monad.State
import           Control.Monad.Error     (throwError)
import           Data.IntMap             (IntMap)
import qualified Data.IntMap             as IntMap
import qualified Data.Map                as Map
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as Text

import qualified Empire.Data.Library     as Library
import qualified Empire.Data.Graph       as Graph
import           Empire.Data.Graph       (Graph)

import           Empire.API.Data.Project  (ProjectId)
import           Empire.API.Data.Library  (LibraryId)
import           Empire.API.Data.Port     (InPort(..), OutPort(..))
import           Empire.API.Data.PortRef  (InPortRef(..), OutPortRef(..))
import qualified Empire.API.Data.PortRef  as PortRef
import           Empire.API.Data.Node     (NodeId, Node(..))
import qualified Empire.API.Data.Node     as Node
import           Empire.API.Data.NodeMeta (NodeMeta)
import qualified Empire.API.Data.Graph    as APIGraph

import           Empire.Empire
import           Empire.Commands.Library      (withLibrary)
import qualified Empire.Commands.AST          as AST
import qualified Empire.Commands.GraphUtils   as GraphUtils
import qualified Empire.Commands.GraphBuilder as GraphBuilder

import qualified Luna.Interpreter.NodeRunner  as NodeRunner

addNode :: ProjectId -> LibraryId -> Text -> NodeMeta -> Empire Node
addNode pid lid expr meta = withGraph pid lid $ do
    newNodeId <- gets Graph.nextNodeId
    refNode <- zoom Graph.ast $ AST.addNode ("node" ++ show newNodeId) (Text.unpack expr)
    zoom Graph.ast $ AST.writeMeta refNode (Just meta)
    Graph.nodeMapping . at newNodeId ?= refNode
    GraphBuilder.buildNode newNodeId

removeNode :: ProjectId -> LibraryId -> NodeId -> Empire ()
removeNode pid lid nodeId = withGraph pid lid $ do
    astRef <- GraphUtils.getASTPointer nodeId
    obsoleteEdges <- getOutEdges nodeId
    mapM_ disconnectPort obsoleteEdges
    zoom Graph.ast $ AST.removeSubtree astRef
    Graph.nodeMapping %= IntMap.delete nodeId

updateNodeMeta :: ProjectId -> LibraryId -> NodeId -> NodeMeta -> Empire ()
updateNodeMeta pid lid nid meta = withGraph pid lid $ do
    ref <- GraphUtils.getASTPointer nid
    zoom Graph.ast $ AST.writeMeta ref $ Just meta

connect :: ProjectId -> LibraryId -> OutPortRef -> InPortRef -> Empire ()
connect pid lid (OutPortRef srcNodeId All) (InPortRef dstNodeId dstPort) = withGraph pid lid $ do
    case dstPort of
        Self    -> makeAcc srcNodeId dstNodeId
        Arg num -> makeApp srcNodeId dstNodeId num
connect _ _ _ _ = throwError "Source port should be All"

disconnect :: ProjectId -> LibraryId -> InPortRef -> Empire ()
disconnect pid lid port = withGraph pid lid $ disconnectPort port

getCode :: ProjectId -> LibraryId -> Empire String
getCode pid lid = withGraph pid lid $ do
    allNodes <- uses Graph.nodeMapping IntMap.keys
    lines <- sequence $ printNodeLine <$> allNodes
    return $ intercalate "\n" lines

getGraph :: ProjectId -> LibraryId -> Empire APIGraph.Graph
getGraph pid lid = withGraph pid lid GraphBuilder.buildGraph

runGraph :: ProjectId -> LibraryId -> Empire (IntMap Int)
runGraph pid lid = withGraph pid lid $ do
    allNodes <- uses Graph.nodeMapping IntMap.keys
    astNodes <- mapM GraphUtils.getASTPointer allNodes
    ast      <- use Graph.ast
    astVals  <- liftIO $ NodeRunner.getNodeValues astNodes ast

    return $ IntMap.fromList $ fmap (\(n, ref) -> (n, Map.findWithDefault 0 ref astVals)) (zip allNodes astNodes)

-- internal

printNodeLine :: NodeId -> Command Graph String
printNodeLine nid = GraphUtils.getASTPointer nid >>= (zoom Graph.ast . AST.printExpression)

withGraph :: ProjectId -> LibraryId -> Command Graph a -> Empire a
withGraph pid lid = withLibrary pid lid . zoom Library.body

getOutEdges :: NodeId -> Command Graph [InPortRef]
getOutEdges nodeId = do
    graphRep <- GraphBuilder.buildGraph
    let edges    = graphRep ^. APIGraph.connections
        filtered = filter (\(opr, _) -> opr ^. PortRef.srcNodeId == nodeId) edges
    return $ view _2 <$> filtered

disconnectPort :: InPortRef -> Command Graph ()
disconnectPort (InPortRef dstNodeId dstPort) = do
    case dstPort of
        Self    -> unAcc dstNodeId
        Arg num -> unApp dstNodeId num

unAcc :: NodeId -> Command Graph ()
unAcc nodeId = do
    dstAst <- GraphUtils.getASTTarget nodeId
    newNodeRef <- zoom Graph.ast $ AST.removeAccessor dstAst
    GraphUtils.rewireNode nodeId newNodeRef

unApp :: NodeId -> Int -> Command Graph ()
unApp nodeId pos = do
    astNode <- GraphUtils.getASTTarget nodeId
    newNodeRef <- zoom Graph.ast $ AST.unapplyArgument astNode pos
    GraphUtils.rewireNode nodeId newNodeRef

makeAcc :: NodeId -> NodeId -> Command Graph ()
makeAcc src dst = do
    srcAst <- GraphUtils.getASTVar src
    dstAst <- GraphUtils.getASTTarget dst
    newNodeRef <- zoom Graph.ast $ AST.makeAccessor srcAst dstAst
    GraphUtils.rewireNode dst newNodeRef

makeApp :: NodeId -> NodeId -> Int -> Command Graph ()
makeApp src dst pos = do
    srcAst <- GraphUtils.getASTVar src
    dstAst <- GraphUtils.getASTTarget dst
    newNodeRef <- zoom Graph.ast $ AST.applyFunction dstAst srcAst pos
    GraphUtils.rewireNode dst newNodeRef
