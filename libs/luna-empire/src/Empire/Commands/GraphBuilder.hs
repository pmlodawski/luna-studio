module Empire.Commands.GraphBuilder where

import           Prologue

import           Control.Monad.State
import           Control.Monad.Error          (throwError)

import qualified Data.IntMap                  as IntMap
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes, fromMaybe, maybeToList)
import qualified Data.Text.Lazy               as Text
import           Data.Record                  (ANY (..), caseTest, match)
import           Data.Layer.Cover             (uncover)
import           Data.Graph                   (source)

import           Empire.Data.Graph            (Graph)
import qualified Empire.Data.Graph            as Graph

import           Empire.API.Data.DefaultValue (PortDefault (..), Value (..))
import qualified Empire.API.Data.Graph        as API
import           Empire.API.Data.Node         (NodeId)
import qualified Empire.API.Data.Node         as API
import           Empire.API.Data.NodeMeta     (NodeMeta (..))
import           Empire.API.Data.Port         (InPort (..), OutPort (..), Port (..), PortId (..), PortState (..))
import           Empire.API.Data.PortRef      (InPortRef (..), OutPortRef (..))
import           Empire.API.Data.ValueType    (ValueType (..))

import           Empire.Data.AST              (NodeRef, EdgeRef)
import           Empire.ASTOp                 (ASTOp, runASTOp)
import qualified Empire.ASTOps.Builder        as ASTBuilder
import qualified Empire.ASTOps.Print          as Print
import qualified Empire.Commands.AST          as AST
import qualified Empire.Commands.GraphUtils   as GraphUtils
import           Empire.Empire

import           Luna.Syntax.AST.Term         (Acc (..), App (..), Blank (..), Unify (..), Var (..))
import qualified Luna.Syntax.AST.Lit          as Lit
import qualified Luna.Syntax.Builder          as Builder

type UniMap = Map NodeRef NodeId

buildGraph :: Command Graph API.Graph
buildGraph = API.Graph <$> buildNodes <*> buildConnections

buildNodes :: Command Graph [API.Node]
buildNodes = do
    allNodeIds <- uses Graph.nodeMapping IntMap.keys
    uniMap     <- getUniMap
    mapM (buildNode' uniMap) allNodeIds

buildNode :: NodeId -> Command Graph API.Node
buildNode nid = getUniMap >>= flip buildNode' nid

buildNode' :: UniMap -> NodeId -> Command Graph API.Node
buildNode' uniMap nodeId = do
    ref   <- GraphUtils.getASTTarget nodeId
    uref  <- GraphUtils.getASTPointer nodeId
    expr  <- zoom Graph.ast $ runASTOp $ Print.printNodeExpression ref
    meta  <- zoom Graph.ast $ AST.readMeta uref
    name  <- getNodeName nodeId
    ports <- buildPorts uniMap ref
    let portMap = Map.fromList $ flip fmap ports $ \p@(Port id _ _) -> (id, p)
    return $ API.Node nodeId (Text.pack name) (API.ExpressionNode $ Text.pack expr) portMap $ fromMaybe def meta

getNodeName :: NodeId -> Command Graph String
getNodeName nid = do
    vref <- GraphUtils.getASTVar nid
    zoom Graph.ast $ runASTOp $ do
        vnode <- Builder.read vref
        caseTest (uncover vnode) $ match $ \(Var n) -> return . toString $ n

getPortState :: ASTOp m => UniMap -> NodeRef -> m PortState
getPortState uniMap ref
    | Map.member ref uniMap = return Connected
    | otherwise             = do
        node <- Builder.read ref
        caseTest (uncover node) $ do
            match $ \(Lit.String s)   -> return . WithDefault . Constant . StringValue $ s
            match $ \(Lit.Number _ n) -> return . WithDefault . Constant $ case n of
                Lit.Integer  i -> IntValue $ fromIntegral i
                Lit.Rational r -> RationalValue r
            match $ \Blank   -> return NotConnected
            match $ \ANY     -> Print.printExpression ref >>= return . WithDefault . Expression

buildPort :: ASTOp m => UniMap -> (PortId, NodeRef) -> m Port
buildPort uniMap (portId, ref) = Port portId (TypeIdent "Int") <$> getPortState uniMap ref

buildSelfPort :: ASTOp m => UniMap -> NodeRef -> m (Maybe Port)
buildSelfPort uniMap nodeRef = do
    node <- Builder.read nodeRef
    caseTest (uncover node) $ do
        match $ \(Acc _ t) -> Builder.follow source t >>= fmap Just . buildPort uniMap . ((,) $ InPortId Self)
        match $ \(App t _) -> Builder.follow source t >>= buildSelfPort uniMap
        match $ \(Var _)   -> return . Just $ Port (InPortId Self) AnyType NotConnected
        match $ \ANY       -> return Nothing

buildPorts :: UniMap -> NodeRef -> Command Graph [Port]
buildPorts uniMap ref = zoom Graph.ast $ runASTOp $ do
    args     <- getPositionalNodeRefs ref
    selfPort <- maybeToList <$> buildSelfPort uniMap ref
    argPorts <- mapM (buildPort uniMap) $ zip (InPortId . Arg <$> [0..]) args
    return $ selfPort ++ argPorts ++ [Port (OutPortId All) (TypeIdent "Int") NotConnected]

buildConnections :: Command Graph [(OutPortRef, InPortRef)]
buildConnections = do
    uniMap <- getUniMap
    allNodes <- uses Graph.nodeMapping IntMap.keys
    edges <- mapM (getNodeInputs uniMap) allNodes
    return $ concat edges

getUniMap :: Command Graph UniMap
getUniMap = do
    let swap (a, b) = (b, a)
    reverseMap <- uses Graph.nodeMapping IntMap.toList
    return $ Map.fromList $ fmap swap reverseMap

getSelfNodeRef' :: ASTOp m => Bool -> NodeRef -> m (Maybe NodeRef)
getSelfNodeRef' seenAcc nodeRef = do
    node <- Builder.read nodeRef
    caseTest (uncover node) $ do
        match $ \(Acc _ t) -> Builder.follow source t >>= getSelfNodeRef' True
        match $ \(App t _) -> Builder.follow source t >>= getSelfNodeRef' seenAcc
        match $ \ANY       -> return $ if seenAcc then Just nodeRef else Nothing

getSelfNodeRef :: ASTOp m => NodeRef -> m (Maybe NodeRef)
getSelfNodeRef = getSelfNodeRef' False

getPositionalNodeRefs :: ASTOp m => NodeRef -> m [NodeRef]
getPositionalNodeRefs nodeRef = do
    node <- Builder.read nodeRef
    caseTest (uncover node) $ do
        match $ \(App _ args) -> ASTBuilder.unpackArguments args
        match $ \ANY          -> return []

getNodeInputs :: UniMap -> NodeId -> Command Graph [(OutPortRef, InPortRef)]
getNodeInputs uniMap nodeId = do
    ref     <- GraphUtils.getASTTarget nodeId
    selfMay <- zoom Graph.ast $ runASTOp $ getSelfNodeRef ref
    let selfNodeMay = selfMay >>= flip Map.lookup uniMap
        selfConnMay = (,) <$> (OutPortRef <$> selfNodeMay <*> Just All)
                          <*> (Just $ InPortRef nodeId Self)

    args <- zoom Graph.ast $ runASTOp $ getPositionalNodeRefs ref
    let nodeMays = flip Map.lookup uniMap <$> args
        withInd  = zip nodeMays [0..]
        onlyExt  = catMaybes $ (\(n, i) -> (,) <$> n <*> Just i) <$> withInd
        conns    = flip fmap onlyExt $ \(n, i) -> (OutPortRef n All, InPortRef nodeId (Arg i))
    return $ (maybeToList selfConnMay) ++ conns
