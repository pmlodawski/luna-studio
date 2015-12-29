module Empire.Commands.Graph
    ( addNode
    , connect
    , disconnect
    ) where

import           Prologue
import           Control.Monad.State

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

addNode :: ProjectId -> LibraryId -> String -> Empire NodeId
addNode pid lid expr = withGraph pid lid $ do
    refNode <- zoom Graph.ast $ AST.addNode expr
    newNodeId <- gets Graph.nextNodeId
    Graph.nodeMapping . at newNodeId ?= refNode
    return newNodeId

connect :: ProjectId -> LibraryId -> NodeId -> OutPort -> NodeId -> InPort -> Empire ()
connect pid lid srcNodeId _ dstNodeId dstPort = withGraph pid lid $ do
    case dstPort of
        Self    -> makeAcc srcNodeId dstNodeId
        Arg num -> makeApp srcNodeId dstNodeId num

disconnect :: ProjectId -> LibraryId -> NodeId -> OutPort -> NodeId -> InPort -> Empire ()
disconnect pid lid _ _ dstNodeId dstPort = withGraph pid lid $ do
    case dstPort of
        Self    -> unAcc dstNodeId
        Arg num -> unApp dstNodeId num

-- implementation

withGraph :: ProjectId -> LibraryId -> Command Graph a -> Empire a
withGraph pid lid = withLibrary pid lid . zoom Library.body

unAcc :: NodeId -> Command Graph ()
unAcc nodeId = do
    dstAst <- use (Graph.nodeMapping . at nodeId) <?!> "Node does not exist"
    nameNode <- zoom Graph.ast $ AST.getNameNode dstAst
    zoom Graph.ast $ AST.removeNode dstAst
    newNodeRef <- zoom Graph.ast $ AST.makeVar nameNode
    Graph.nodeMapping . at nodeId ?= newNodeRef
    return ()

unApp :: NodeId -> Int -> Command Graph ()
unApp nodeId pos = do
    astNode <- use (Graph.nodeMapping . at nodeId) <?!> "Node does not exist"
    newNodeRef <- zoom Graph.ast $ AST.removeArg astNode pos
    Graph.nodeMapping . at nodeId ?= newNodeRef

makeAcc :: NodeId -> NodeId -> Command Graph ()
makeAcc src dst = do
    srcAst <- use (Graph.nodeMapping . at src) <?!> "Source node does not exist"
    dstAst <- use (Graph.nodeMapping . at dst) <?!> "Destination node does not exist"
    name <- zoom Graph.ast $ AST.getNameNode dstAst
    zoom Graph.ast $ AST.removeNode dstAst
    newNodeRef <- zoom Graph.ast $ AST.makeAccessor srcAst name
    Graph.nodeMapping . at dst ?= newNodeRef
    return ()

makeApp :: NodeId -> NodeId -> Int -> Command Graph ()
makeApp src dst pos = do
    srcAst <- use (Graph.nodeMapping . at src) <?!> "Source node does not exist"
    dstAst <- use (Graph.nodeMapping . at dst) <?!> "Destination node does not exist"
    newNodeRef <- zoom Graph.ast $ AST.applyFunction dstAst srcAst pos
    Graph.nodeMapping . at dst ?= newNodeRef
    return ()
