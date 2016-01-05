module Empire.Commands.Graph
    ( addNode
    , connect
    , disconnect
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
import           Empire.API.Data.Node    (NodeId)

import           Empire.Empire
import           Empire.Commands.Library (withLibrary)
import qualified Empire.Commands.AST     as AST
import           Luna.Syntax.Repr.Graph  (Ref, Node)

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

-- internal

withGraph :: ProjectId -> LibraryId -> Command Graph a -> Empire a
withGraph pid lid = withLibrary pid lid . zoom Library.body

unAcc :: NodeId -> Command Graph ()
unAcc nodeId = do
    dstAst <- use (Graph.nodeMapping . at nodeId) <?!> "Node does not exist"
    nameNode <- zoom Graph.ast $ AST.getNameNode dstAst
    zoom Graph.ast $ AST.removeNode dstAst
    newNodeRef <- zoom Graph.ast $ AST.makeVar nameNode
    Graph.nodeMapping . at nodeId ?= newNodeRef

unApp :: NodeId -> Int -> Command Graph ()
unApp nodeId pos = do
    astNode <- use (Graph.nodeMapping . at nodeId) <?!> "Node does not exist"
    newNodeRef <- zoom Graph.ast $ AST.removeArg astNode pos
    Graph.nodeMapping . at nodeId ?= newNodeRef

getASTPointer :: NodeId -> Command Graph (Ref Node)
getASTPointer nodeId = use (Graph.nodeMapping . at nodeId) <?!> "Node does not exist"

getASTTarget :: NodeId -> Command Graph (Ref Node)
getASTTarget nodeId = do
    unifyNode <- getASTPointer nodeId
    zoom Graph.ast $ AST.getTargetNode unifyNode

getASTVar :: NodeId -> Command Graph (Ref Node)
getASTVar nodeId = do
    unifyNode <- getASTPointer nodeId
    zoom Graph.ast $ AST.getVarNode unifyNode

rewireNode :: NodeId -> Ref Node -> Command Graph ()
rewireNode nodeId newTarget = do
    unifyNode <- getASTPointer nodeId
    zoom Graph.ast $ AST.replaceTargetNode unifyNode newTarget

makeAcc :: NodeId -> NodeId -> Command Graph ()
makeAcc src dst = do
    srcAst <- getASTVar src
    dstAst <- getASTTarget dst
    name <- zoom Graph.ast $ AST.getNameNode dstAst
    zoom Graph.ast $ AST.removeNode dstAst
    newNodeRef <- zoom Graph.ast $ AST.makeAccessor srcAst name
    rewireNode dst newNodeRef


makeApp :: NodeId -> NodeId -> Int -> Command Graph ()
makeApp src dst pos = do
    srcAst <- getASTVar src
    dstAst <- getASTTarget dst
    newNodeRef <- zoom Graph.ast $ AST.applyFunction dstAst srcAst pos
    rewireNode dst newNodeRef
