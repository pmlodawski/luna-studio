{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
module Main where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.ByteString.Lazy        as BS
import Data.Map (Map)
import Prologue
import Data.Aeson
import Empire.Commands.Autolayout (Connection)
import           LunaStudio.Data.Position  (Position, x, y)
import qualified Temp as Temp
import Temp (VisNode (VisNode), VisEdge (VisEdge), VisGraph (VisGraph))
import LunaStudio.Data.PortRef
import LunaStudio.Data.Port
import LunaStudio.Data.Node (NodeId)
import LunaStudio.Data.NodeLoc (nodeId)
import           Data.UUID                     (UUID)
import           Data.UUID.V4                  (nextRandom)


mkUUID :: MonadIO m => m UUID
mkUUID = liftIO nextRandom

mkVisNode :: Int -> Position -> VisNode
mkVisNode id pos = VisNode id (round $ pos ^. x) (round $ pos ^. y) True

mkVisEdge :: Int -> Int -> Connection -> VisEdge
mkVisEdge srcId dstId conn = VisEdge srcId dstId "to" $ toString conn where
    toString (OutPortRef _ srcPid, InPortRef _ dstPid) = (show (getPortNumber srcPid)) <> " -> " <> (if isSelf dstPid then "self" else show $ getPortNumber dstPid)

mkVisGraph :: [(NodeId, Position)] -> [Connection] -> VisGraph
mkVisGraph nodes conns = do
    let nodesMap = foldl (\acc (nid, pos) -> if Map.member nid acc then acc else Map.insert nid (Map.size acc, pos) acc) mempty nodes
        visNodes = (\(nid, pos) -> mkVisNode nid pos) <$> Map.elems nodesMap
        fromConn conn@(src, dst) = (\(i1, _) (i2, _) -> mkVisEdge i1 i2 conn) <$> Map.lookup (src ^. srcNodeId) nodesMap <*> Map.lookup (dst ^. dstNodeId) nodesMap
        visConns = catMaybes $ fromConn <$> conns
    VisGraph visNodes $ visConns

mkNodes :: Int -> IO [(NodeId, Position)]
mkNodes 0    = return def
mkNodes size = ((, def) <$> mkUUID) >>= \h -> (h:) <$> mkNodes (size - 1)

main :: IO ()
main = do
    nodes <- mkNodes 3
    let nid1  = fst $ List.head nodes
        nid2  = fst . List.head $ List.tail nodes
    let conns = [(OutPortRef (convert nid1) def, InPortRef (convert nid2) def)]
    print $ mkVisGraph nodes conns
    print . encode $ mkVisGraph nodes conns

    print "AUTOLAYOUT"
