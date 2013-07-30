---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module GraphHandler (
graph,
addNode,
updateNode,
removeNode,
connect,
disconnect
) 
where

import qualified Graph_Types as TGraph



graph a definition = do
    putStrLn "NOT IMPLEMENTED - graph"
    return $ TGraph.Graph Nothing Nothing


addNode a node definition = do
    putStrLn "NOT IMPLEMENTED - addNode"
    return $ TGraph.Node Nothing Nothing Nothing Nothing Nothing


updateNode a node definition = do
    putStrLn "NOT IMPLEMENTED - updateNode"


removeNode a node definition = do
    putStrLn "NOT IMPLEMENTED - removeNode"


connect    a srcNode srcPort dstNode dstPort definition = do 
    putStrLn "NOT IMPLEMENTED - connect"


disconnect a srcNode srcPort dstNode dstPort definition = do 
    putStrLn "NOT IMPLEMENTED - disconnect"
