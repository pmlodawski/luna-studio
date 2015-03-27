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

import           Control.Monad.State hiding (mapM)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import qualified Data.Maybe          as Maybe

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Luna.Syntax.Enum          as Enum
import qualified Luna.Syntax.Graph.Edge    as Edge
import           Luna.Syntax.Graph.Graph   (Graph)
import qualified Luna.Syntax.Graph.Graph   as Graph
import qualified Luna.Syntax.Graph.Node    as Node
import           Luna.Syntax.Graph.Port    (DstPort, SrcPort)
import           Luna.Syntax.Graph.Tag     (TExpr, Tag)
import           Luna.Syntax.Label         (Label (Label))



dummyLabel :: a -> Label Tag a
dummyLabel = Label (Enum.tag (-999))


logger :: Logger
logger = getLogger $moduleName


type Error = String

type ExprMap v = Map (Node.ID, SrcPort) (TExpr v)


data GPState v = GPState { _body   :: [TExpr v]
                         , _output :: Maybe (TExpr v)
                         , _varMap :: ExprMap v
                         , _graph  :: Graph Tag v
                         } deriving (Show)

makeLenses ''GPState


type GPPass v m result = Monad m => StateT (GPState v) (EitherT Error m) result


mk :: Graph Tag v -> GPState v
mk = GPState def def def


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

addToExprMap :: (Node.ID, SrcPort) -> TExpr v -> GPPass v m ()
addToExprMap key expr = getExprMap >>= setExprMap . Map.insert key expr

exprMapLookup :: (Node.ID, SrcPort) -> GPPass v m (TExpr v)
exprMapLookup key = do
    nm <- getExprMap
    lift $ Map.lookup key nm <??> "GraphParser: exprMapLookup: Cannot find " ++ show key ++ " in exprMap"

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


--getNode :: Node.ID -> GPPass a v m (Node a v)
--getNode nodeID = do
--    gr <- getGraph
--    lift $ Graph.lab gr nodeID <??> "GraphParser: getNodeOutputName: Cannot find nodeID=" ++ show nodeID ++ " in graph"


--getNodeOutputName :: Node.ID -> GPPass a v m VNameP
--getNodeOutputName nodeID = do
--    outputName <- preview Node.outputName <$> getNode nodeID
--    lift $ outputName <??> "GraphParser: getNodeOutputName"


--getFlags :: Node.ID -> GPPass a v m Flags
--getFlags nodeID = PropertyMap.getFlags nodeID <$> getPropertyMap


--modifyFlags :: (Flags -> Flags) -> Node.ID -> GPPass a v m ()
--modifyFlags fun nodeID =
--    getPropertyMap >>= setPropertyMap . PropertyMap.modifyFlags fun nodeID


--setPosition :: Node.ID -> Position -> GPPass a v m ()
--setPosition nodeID position =
--    modifyFlags (Flags.nodePosition .~ Just position) nodeID


--setGraphFolded :: Node.ID -> GPPass a v m ()
--setGraphFolded = modifyFlags (Flags.graphFoldInfo .~ Just Flags.Folded)


--setGraphFoldTop :: Node.ID -> Node.ID -> GPPass a v m ()
--setGraphFoldTop nodeID topID =
--    modifyFlags (Flags.graphFoldInfo .~ Just (Flags.FoldTop topID)) nodeID
