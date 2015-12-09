module Empire.Commands.Graph where

import           Prologue
import           Control.Monad.State

import           Empire.Data.Project     (ProjectId)
import           Empire.Data.Library     (LibraryId)
import qualified Empire.Data.Library     as Library
import qualified Empire.Data.Graph       as Graph
import           Empire.Data.Graph       (Graph, NodeId)
import           Empire.Data.Port        (InPort(..), OutPort(..))

import           Empire.Empire
import           Empire.Commands.Library (withLibrary)
import qualified Empire.Commands.AST     as AST

withGraph :: ProjectId -> LibraryId -> Command Graph a -> Empire a
withGraph pid lid = withLibrary pid lid . zoom Library.body

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

makeAcc :: NodeId -> NodeId -> Command Graph ()
makeAcc src dst = do
    srcAst <- use (Graph.nodeMapping . at src) <?!> "Source node does not exist"
    dstAst <- use (Graph.nodeMapping . at dst) <?!> "Destination node does not exist"
    name <- zoom Graph.ast $ AST.getVarNameNode dstAst
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
