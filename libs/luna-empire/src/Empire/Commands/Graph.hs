module Empire.Commands.Graph
    ( addNode
    , removeNode
    , updateNodeMeta
    , connect
    , disconnect
    , getCode
    , getGraph
    , runGraph
    , setDefaultValue
    , renameNode
    , dumpGraphViz
    ) where

import           Prologue
import           Control.Monad.State
import           Unsafe.Coerce           (unsafeCoerce)
import           Control.Monad.Error     (throwError)
import           Control.Monad           (forM)
import           Data.IntMap             (IntMap)
import qualified Data.IntMap             as IntMap
import qualified Data.Map                as Map
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as Text
import           Data.Maybe              (catMaybes)

import qualified Empire.Data.Library     as Library
import qualified Empire.Data.Graph       as Graph
import           Empire.Data.Graph       (Graph)

import           Empire.API.Data.Project       (ProjectId)
import           Empire.API.Data.Library       (LibraryId)
import           Empire.API.Data.Port          (InPort(..), OutPort(..))
import           Empire.API.Data.PortRef       (InPortRef(..), OutPortRef(..))
import qualified Empire.API.Data.PortRef       as PortRef
import           Empire.API.Data.Node          (NodeId, Node(..))
import qualified Empire.API.Data.Node          as Node
import           Empire.API.Data.NodeMeta      (NodeMeta)
import qualified Empire.API.Data.Graph         as APIGraph
import           Empire.API.Data.DefaultValue  (PortDefault, Value(..))
import           Empire.API.Data.GraphLocation (GraphLocation (..))

import           Empire.Empire
import           Empire.Commands.Library      (withLibrary)
import qualified Empire.Commands.AST          as AST
import qualified Empire.Commands.GraphUtils   as GraphUtils
import qualified Empire.Commands.GraphBuilder as GraphBuilder
import qualified Empire.Commands.Publisher    as Publisher

addNode :: GraphLocation -> Text -> NodeMeta -> Empire Node
addNode loc expr meta = withGraph loc $ do
    newNodeId <- gets Graph.nextNodeId
    refNode <- zoom Graph.ast $ AST.addNode ("node" ++ show newNodeId) (Text.unpack expr)
    zoom Graph.ast $ AST.writeMeta refNode (Just meta)
    Graph.nodeMapping . at newNodeId ?= refNode
    GraphBuilder.buildNode newNodeId

removeNode :: GraphLocation -> NodeId -> Empire ()
removeNode loc nodeId = withGraph loc $ do
    astRef <- GraphUtils.getASTPointer nodeId
    obsoleteEdges <- getOutEdges nodeId
    mapM_ (disconnectPort $ Publisher.notifyNodeUpdate loc) obsoleteEdges
    zoom Graph.ast $ AST.removeSubtree astRef
    Graph.nodeMapping %= IntMap.delete nodeId

updateNodeMeta :: GraphLocation -> NodeId -> NodeMeta -> Empire ()
updateNodeMeta loc nodeId meta = withGraph loc $ do
    ref <- GraphUtils.getASTPointer nodeId
    zoom Graph.ast $ AST.writeMeta ref $ Just meta

connect :: GraphLocation -> OutPortRef -> InPortRef -> Empire ()
connect loc (OutPortRef srcNodeId All) (InPortRef dstNodeId dstPort) = withGraph loc $ do
    case dstPort of
        Self    -> makeAcc srcNodeId dstNodeId
        Arg num -> makeApp srcNodeId dstNodeId num
    GraphBuilder.buildNode dstNodeId >>= Publisher.notifyNodeUpdate loc
connect _ _ _ = throwError "Source port should be All"

setDefaultValue :: GraphLocation -> InPortRef -> PortDefault -> Empire ()
setDefaultValue loc (InPortRef nodeId port) val = withGraph loc $ do
    ref <- GraphUtils.getASTTarget nodeId
    parsed <- zoom Graph.ast $ AST.addDefault val
    newRef <- zoom Graph.ast $ case port of
        Self    -> AST.makeAccessor parsed ref
        Arg num -> AST.applyFunction ref parsed num
    GraphUtils.rewireNode nodeId newRef
    node <- GraphBuilder.buildNode nodeId
    Publisher.notifyNodeUpdate loc node

disconnect :: GraphLocation -> InPortRef -> Empire ()
disconnect loc port@(InPortRef dstNodeId dstPort) = withGraph loc $ do
    disconnectPort (Publisher.notifyNodeUpdate loc) port

getCode :: GraphLocation -> Empire String
getCode loc = withGraph loc $ do
    allNodes <- uses Graph.nodeMapping IntMap.keys
    lines <- sequence $ printNodeLine <$> allNodes
    return $ intercalate "\n" lines

getGraph :: GraphLocation -> Empire APIGraph.Graph
getGraph loc = withGraph loc GraphBuilder.buildGraph

runGraph :: GraphLocation -> Empire (IntMap Value)
runGraph loc = withGraph loc $ do
    {-allNodes <- uses Graph.nodeMapping IntMap.keys-}
    {-astNodes <- mapM GraphUtils.getASTPointer allNodes-}
    {-ast      <- use Graph.ast-}
    {-astVals  <- liftIO $ NodeRunner.getNodeValues astNodes ast-}

    {-let values = flip fmap (zip allNodes astNodes) $ \(n, ref) -> do-}
        {-val <- Map.lookup ref astVals-}
        {-case val of-}
            {-NodeRunner.HaskellVal v tp -> return $ (,) n $ case tp of-}
                {-"Int"    -> IntValue $ unsafeCoerce v-}
                {-"Double" -> DoubleValue $ unsafeCoerce v-}
                {-"String" -> StringValue $ unsafeCoerce v-}
                {-"[Int]"  -> IntList $ unsafeCoerce v-}
                {-"[Double]" -> DoubleList $ unsafeCoerce v-}

            {-_ -> Nothing-}

    {-return $ IntMap.fromList $ catMaybes values-}
    return $ IntMap.empty

renameNode :: GraphLocation -> NodeId -> Text -> Empire ()
renameNode loc nid name = withGraph loc $ do
    vref <- GraphUtils.getASTVar nid
    zoom Graph.ast $ AST.renameVar vref (Text.unpack name)
    GraphBuilder.buildNode nid >>= Publisher.notifyNodeUpdate loc

dumpGraphViz :: GraphLocation -> Empire ()
dumpGraphViz loc = withGraph loc $ zoom Graph.ast $ AST.dumpGraphViz

-- internal

printNodeLine :: NodeId -> Command Graph String
printNodeLine nodeId = GraphUtils.getASTPointer nodeId >>= (zoom Graph.ast . AST.printExpression)

withGraph :: GraphLocation -> Command Graph a -> Empire a
withGraph (GraphLocation pid lid _) = withLibrary pid lid . zoom Library.body

getOutEdges :: NodeId -> Command Graph [InPortRef]
getOutEdges nodeId = do
    graphRep <- GraphBuilder.buildGraph
    let edges    = graphRep ^. APIGraph.connections
        filtered = filter (\(opr, _) -> opr ^. PortRef.srcNodeId == nodeId) edges
    return $ view _2 <$> filtered

disconnectPort :: (Node -> Command Graph ()) -> InPortRef -> Command Graph ()
disconnectPort notif (InPortRef dstNodeId dstPort) = do
    case dstPort of
        Self    -> unAcc dstNodeId
        Arg num -> unApp dstNodeId num
    GraphBuilder.buildNode dstNodeId >>= notif

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
    srcAst <- GraphUtils.getASTPointer src
    dstAst <- GraphUtils.getASTTarget dst
    newNodeRef <- zoom Graph.ast $ AST.makeAccessor srcAst dstAst
    GraphUtils.rewireNode dst newNodeRef

makeApp :: NodeId -> NodeId -> Int -> Command Graph ()
makeApp src dst pos = do
    srcAst <- GraphUtils.getASTPointer src
    dstAst <- GraphUtils.getASTTarget dst
    newNodeRef <- zoom Graph.ast $ AST.applyFunction dstAst srcAst pos
    GraphUtils.rewireNode dst newNodeRef
