module Empire.Commands.Graph
    ( addNode
    , removeNode
    , updateNodeMeta
    , connect
    , disconnect
    , getCode
    , getGraph
    , setDefaultValue
    , renameNode
    , dumpGraphViz
    , typecheck
    ) where

import           Prologue
import           Control.Monad.State
import           Unsafe.Coerce           (unsafeCoerce)
import           Control.Monad.Error     (throwError)
import           Control.Monad           (forM, forM_)
import           Data.IntMap             (IntMap)
import qualified Data.IntMap             as IntMap
import qualified Data.Map                as Map
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as Text
import           Data.Maybe              (catMaybes)
import           Data.List               (sort)

import qualified Empire.Data.Library     as Library
import qualified Empire.Data.Graph       as Graph
import           Empire.Data.Graph       (Graph)

import           Empire.API.Data.Project       (ProjectId)
import           Empire.API.Data.Library       (LibraryId)
import           Empire.API.Data.Port          (InPort(..), OutPort(..))
import           Empire.API.Data.PortRef       (InPortRef(..), OutPortRef(..), AnyPortRef(..))
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

addNode :: GraphLocation -> Text -> NodeMeta -> Empire NodeId
addNode loc expr meta = withGraph loc $ do
    newNodeId <- gets Graph.nextNodeId
    refNode <- zoom Graph.ast $ AST.addNode newNodeId ("node" ++ show newNodeId) (Text.unpack expr)
    zoom Graph.ast $ AST.writeMeta refNode meta
    Graph.nodeMapping . at newNodeId ?= refNode
    node <- GraphBuilder.buildNode newNodeId
    Publisher.notifyNodeUpdate loc node
    runTC loc False
    return newNodeId

removeNode :: GraphLocation -> NodeId -> Empire ()
removeNode loc nodeId = withGraph loc $ do
    astRef <- GraphUtils.getASTPointer nodeId
    obsoleteEdges <- getOutEdges nodeId
    mapM_ disconnectPort obsoleteEdges
    zoom Graph.ast $ AST.removeSubtree astRef
    Graph.nodeMapping %= IntMap.delete nodeId
    runTC loc False

updateNodeMeta :: GraphLocation -> NodeId -> NodeMeta -> Empire ()
updateNodeMeta loc nodeId meta = withGraph loc $ do
    ref <- GraphUtils.getASTPointer nodeId
    zoom Graph.ast $ AST.writeMeta ref meta

connect :: GraphLocation -> OutPortRef -> InPortRef -> Empire ()
connect loc (OutPortRef srcNodeId All) (InPortRef dstNodeId dstPort) = withGraph loc $ do
    case dstPort of
        Self    -> makeAcc srcNodeId dstNodeId
        Arg num -> makeApp srcNodeId dstNodeId num
    runTC loc False
connect _ _ _ = throwError "Source port should be All"

setDefaultValue :: GraphLocation -> AnyPortRef -> PortDefault -> Empire ()
setDefaultValue loc portRef val = withGraph loc $ do
    parsed <- zoom Graph.ast $ AST.addDefault val
    (nodeId, newRef) <- case portRef of
        InPortRef' (InPortRef nodeId port) -> do
            ref <- GraphUtils.getASTTarget nodeId
            newRef <- zoom Graph.ast $ case port of
                Self    -> AST.makeAccessor parsed ref
                Arg num -> AST.applyFunction ref parsed num
            return (nodeId, newRef)
        OutPortRef' (OutPortRef nodeId _) -> return (nodeId, parsed)
    GraphUtils.rewireNode nodeId newRef
    runTC loc False

disconnect :: GraphLocation -> InPortRef -> Empire ()
disconnect loc port@(InPortRef dstNodeId dstPort) = withGraph loc $ do
    disconnectPort port
    runTC loc False

getCode :: GraphLocation -> Empire String
getCode loc = withGraph loc $ do
    allNodes <- uses Graph.nodeMapping IntMap.keys
    refs     <- mapM GraphUtils.getASTPointer allNodes
    metas    <- zoom Graph.ast $ mapM AST.readMeta refs
    let sorted = fmap snd $ sort $ zip metas allNodes
    lines <- sequence $ printNodeLine <$> sorted
    return $ unlines lines

getGraph :: GraphLocation -> Empire APIGraph.Graph
getGraph loc = withGraph loc $ runTC loc True >> GraphBuilder.buildGraph

renameNode :: GraphLocation -> NodeId -> Text -> Empire ()
renameNode loc nid name = withGraph loc $ do
    vref <- GraphUtils.getASTVar nid
    zoom Graph.ast $ AST.renameVar vref (Text.unpack name)
    runTC loc False

dumpGraphViz :: GraphLocation -> Empire ()
dumpGraphViz loc = withGraph loc $ do
    zoom Graph.ast   $ AST.dumpGraphViz "gui_dump"
    {-zoom Graph.tcAST $ AST.dumpGraphViz "gui_tc_dump"-}

typecheck :: GraphLocation -> Empire ()
typecheck loc = withGraph loc $ runTC loc False

-- internal

runTC :: GraphLocation -> Bool -> Command Graph ()
runTC loc flush = do
    g <- get
    Publisher.requestTC loc g flush

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
    srcAst <- GraphUtils.getASTVar    src
    dstAst <- GraphUtils.getASTTarget dst
    newNodeRef <- zoom Graph.ast $ AST.makeAccessor srcAst dstAst
    GraphUtils.rewireNode dst newNodeRef

makeApp :: NodeId -> NodeId -> Int -> Command Graph ()
makeApp src dst pos = do
    srcAst <- GraphUtils.getASTVar    src
    dstAst <- GraphUtils.getASTTarget dst
    newNodeRef <- zoom Graph.ast $ AST.applyFunction dstAst srcAst pos
    GraphUtils.rewireNode dst newNodeRef
