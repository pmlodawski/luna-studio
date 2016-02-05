 ---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Luna.Pass.Transform.Graph.Parser.State where

import           Control.Monad.State             hiding (mapM)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import qualified Data.Maybe                      as Maybe

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Luna.Data.ASTInfo               (ASTInfo)
import qualified Luna.Data.ASTInfo               as ASTInfo
import qualified Luna.Syntax.AST                 as AST
import qualified Luna.Syntax.Graph.Edge          as Edge
import           Luna.Syntax.Graph.Graph         (Graph)
import qualified Luna.Syntax.Graph.Graph         as Graph
import qualified Luna.Syntax.Graph.Node          as Node
import           Luna.Syntax.Graph.Node.Position (Position)
import           Luna.Syntax.Graph.Port          (DstPort, SrcPort)
import           Luna.Syntax.Graph.Tag           (TExpr, Tag)



logger :: Logger
logger = getLogger $moduleName


type Error = String

type ExprMap v = Map (Node.ID, SrcPort) (State ASTInfo (TExpr v))

type GPPass v m result = MonadIO m => StateT (GPState v) (EitherT Error m) result

data GPState v = GPState { _body          :: [TExpr v]
                         , _output        :: Maybe (TExpr v)
                         , _varMap        :: ExprMap v
                         , _inputsPos     :: Position
                         , _outputsPos    :: Position
                         , _highestNodeID :: Node.ID
                         , _graph         :: Graph Tag v
                         , _astInfo       :: ASTInfo
                         }

makeLenses ''GPState


mk :: Graph Tag v -> ASTInfo -> GPState v
mk = GPState def def def def def def

----- body ----------------------------------------------------------------
getBody :: GPPass v m [TExpr v]
getBody = gets $ view body

setBody :: [TExpr v] -> GPPass v m ()
setBody = modify . set body

addToBody :: TExpr v -> GPPass v m ()
addToBody e = setBody . (e :) =<< getBody

----- output --------------------------------------------------------------
getOutput :: GPPass v m (Maybe (TExpr v))
getOutput = gets $ view output

setOutput :: TExpr v -> GPPass v m ()
setOutput = modify . set output . Just

----- varMap -------------------------------------------------------------
getExprMap :: GPPass v m (ExprMap v)
getExprMap = gets $ view varMap

setExprMap :: ExprMap v -> GPPass v m ()
setExprMap = modify . set varMap

addToExprMap :: (Node.ID, SrcPort) -> State ASTInfo (TExpr v) -> GPPass v m ()
addToExprMap key expr = getExprMap >>= setExprMap . Map.insert key expr

exprMapLookup :: (Node.ID, SrcPort) -> GPPass v m (TExpr v)
exprMapLookup key = do
    nm <- getExprMap
    r <- lift $ Map.lookup key nm <??> "GraphParser: exprMapLookup: Cannot find " ++ show key ++ " in exprMap"
    withASTInfo r

----- inputsPos -----------------------------------------------------------
setInputsPos :: Position -> GPPass v m ()
setInputsPos = modify . set inputsPos

getInputsPos :: GPPass v m Position
getInputsPos = gets $ view inputsPos

----- outputsPos ----------------------------------------------------------
setOutputsPos :: Position -> GPPass v m ()
setOutputsPos = modify . set outputsPos

getOutputsPos :: GPPass v m Position
getOutputsPos = gets $ view outputsPos

----- highestNodeID -------------------------------------------------------
reportID :: Node.ID -> GPPass v m ()
reportID = modify . over highestNodeID . max

getHighestID :: GPPass v m Node.ID
getHighestID = gets $ view highestNodeID

----- graph ---------------------------------------------------------------
getGraph :: GPPass v m (Graph Tag v)
getGraph = gets $ view graph

inboundPorts :: Node.ID -> GPPass v m [DstPort]
inboundPorts nodeID = do
    g <- getGraph
    let processEdge (_, Edge.Data _ d) = Just d
        processEdge (_, Edge.Monadic ) = Nothing
    return $ Maybe.mapMaybe processEdge
           $ Graph.lpre g nodeID

----- astInfo -------------------------------------------------------------
getASTInfo :: GPPass v m ASTInfo
getASTInfo = gets $ view astInfo

setASTInfo :: ASTInfo -> GPPass v m ()
setASTInfo = modify . set astInfo

nextID :: GPPass v m AST.ID
nextID = do
    i <- getASTInfo
    let n = ASTInfo.incID i
    setASTInfo n
    return $ n ^. ASTInfo.lastID

withASTInfo :: State ASTInfo a -> GPPass v m a
withASTInfo s = do
    i <- getASTInfo
    let (r, i') = runState s i
    setASTInfo i'
    return r
