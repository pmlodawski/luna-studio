module Empire.Commands.GraphBuilder where

import           Prologue

import           Control.Monad.State
import           Control.Monad.Error          (throwError)

import qualified Data.IntMap                  as IntMap
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes, fromMaybe, maybeToList)
import qualified Data.Text.Lazy               as Text
import           Data.Record                  (ANY (..), caseTest, of')
import           Data.Layer.Cover             (uncover)
import           Data.Graph                   (source)
import           Data.Prop                    (prop)

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

import           Luna.Syntax.AST.Term         (Acc (..), App (..), Blank (..), Match (..), Var (..), Cons (..), Lam (..))
import qualified Luna.Syntax.AST.Lit          as Lit
import qualified Luna.Syntax.Builder          as Builder
import           Luna.Syntax.Builder          (Type (..))


buildGraph :: Command Graph API.Graph
buildGraph = API.Graph <$> buildNodes <*> buildConnections

buildNodes :: Command Graph [API.Node]
buildNodes = do
    allNodeIds <- uses Graph.nodeMapping IntMap.keys
    mapM buildNode allNodeIds

buildNode :: NodeId -> Command Graph API.Node
buildNode nid = do
    ref   <- GraphUtils.getASTTarget nid
    uref  <- GraphUtils.getASTPointer nid
    expr  <- zoom Graph.tcAST $ runASTOp $ Print.printNodeExpression ref
    meta  <- zoom Graph.tcAST $ AST.readMeta uref
    name  <- getNodeName nid
    ports <- buildPorts ref
    let portMap = Map.fromList $ flip fmap ports $ \p@(Port id _ _) -> (id, p)
    return $ API.Node nid (Text.pack name) (API.ExpressionNode $ Text.pack expr) portMap $ fromMaybe def meta

getNodeName :: NodeId -> Command Graph String
getNodeName nid = do
    vref <- GraphUtils.getASTVar nid
    zoom Graph.tcAST $ runASTOp $ do
        vnode <- Builder.read vref
        caseTest (uncover vnode) $ of' $ \(Var n) -> return . toString $ n

getPortState :: ASTOp m => NodeRef -> m PortState
getPortState ref = do
    isConnected <- ASTBuilder.isGraphNode ref
    if isConnected then return Connected else do
        node <- Builder.read ref
        caseTest (uncover node) $ do
            of' $ \(Lit.String s)   -> return . WithDefault . Constant . StringValue $ s
            of' $ \(Lit.Number _ n) -> return . WithDefault . Constant $ case n of
                Lit.Integer  i -> IntValue $ fromIntegral i
                Lit.Rational r -> RationalValue r
            of' $ \Blank   -> return NotConnected
            of' $ \ANY     -> Print.printExpression ref >>= return . WithDefault . Expression

extractArgTypes :: ASTOp m => NodeRef -> m [ValueType]
extractArgTypes ref = do
    node <- Builder.read ref
    caseTest (uncover node) $ do
        of' $ \(Lam args _) -> do
            unpacked <- ASTBuilder.unpackArguments args
            mapM getTypeRep unpacked
        of' $ \ANY -> return []


buildArgPorts :: ASTOp m => NodeRef -> m [Port]
buildArgPorts ref = do
    node <- Builder.read ref
    (types, states) <- caseTest (uncover node) $ do
        of' $ \(App f args) -> do
            tpRef      <- Builder.follow source =<< Builder.follow (prop Type) =<< Builder.follow source f
            portTypes  <- extractArgTypes tpRef
            unpacked   <- ASTBuilder.unpackArguments args
            portStates <- mapM getPortState unpacked
            return (portTypes, portStates)
        of' $ \(Var _) -> do
            tpRef <- Builder.follow source $ node ^. prop Type
            types <- extractArgTypes tpRef
            return (types, [])
        of' $ \ANY -> return ([], [])
    let psCons = zipWith Port (InPortId . Arg <$> [0..]) types
    return $ zipWith ($) psCons $ states ++ repeat NotConnected

buildSelfPort :: ASTOp m => NodeRef -> m (Maybe Port)
buildSelfPort nodeRef = do
    node <- Builder.read nodeRef
    caseTest (uncover node) $ do
        of' $ \(Acc _ t) -> Builder.follow source t >>= buildSelfPort
        of' $ \(App t _) -> Builder.follow source t >>= buildSelfPort
        of' $ \(Var _)   -> do
            tpRep     <- followTypeRep nodeRef
            portState <- getPortState nodeRef
            return . Just $ Port (InPortId Self) tpRep portState
        of' $ \ANY       -> return Nothing

followTypeRep :: ASTOp m => NodeRef -> m ValueType
followTypeRep ref = do
    tp <- Builder.follow source =<< Builder.follow (prop Type) ref
    getTypeRep tp

getTypeRep :: ASTOp m => NodeRef -> m ValueType
getTypeRep tp = do
    tpNode <- Builder.read tp
    caseTest (uncover tpNode) $ do
        of' $ \(Cons (Lit.String s)) -> return $ TypeIdent s
        of' $ \ANY -> return AnyType

buildPorts :: NodeRef -> Command Graph [Port]
buildPorts ref = zoom Graph.tcAST $ runASTOp $ do
    selfPort <- maybeToList <$> buildSelfPort ref
    argPorts <- buildArgPorts ref
    tpRep <- followTypeRep ref
    return $ selfPort ++ argPorts ++ [Port (OutPortId All) tpRep NotConnected]

buildConnections :: Command Graph [(OutPortRef, InPortRef)]
buildConnections = do
    allNodes <- uses Graph.nodeMapping IntMap.keys
    edges <- mapM getNodeInputs allNodes
    return $ concat edges

getSelfNodeRef' :: ASTOp m => Bool -> NodeRef -> m (Maybe NodeRef)
getSelfNodeRef' seenAcc nodeRef = do
    node <- Builder.read nodeRef
    caseTest (uncover node) $ do
        of' $ \(Acc _ t) -> Builder.follow source t >>= getSelfNodeRef' True
        of' $ \(App t _) -> Builder.follow source t >>= getSelfNodeRef' seenAcc
        of' $ \ANY       -> return $ if seenAcc then Just nodeRef else Nothing

getSelfNodeRef :: ASTOp m => NodeRef -> m (Maybe NodeRef)
getSelfNodeRef = getSelfNodeRef' False

getPositionalNodeRefs :: ASTOp m => NodeRef -> m [NodeRef]
getPositionalNodeRefs nodeRef = do
    node <- Builder.read nodeRef
    caseTest (uncover node) $ do
        of' $ \(App _ args) -> ASTBuilder.unpackArguments args
        of' $ \ANY          -> return []

getNodeInputs :: NodeId -> Command Graph [(OutPortRef, InPortRef)]
getNodeInputs nodeId = do
    ref         <- GraphUtils.getASTTarget nodeId
    selfMay     <- zoom Graph.tcAST $ runASTOp $ getSelfNodeRef ref
    selfNodeMay <- case selfMay of
        Just self -> zoom Graph.tcAST $ runASTOp $ ASTBuilder.getNodeId self
        Nothing   -> return Nothing
    let selfConnMay = (,) <$> (OutPortRef <$> selfNodeMay <*> Just All)
                          <*> (Just $ InPortRef nodeId Self)

    args <- zoom Graph.tcAST $ runASTOp $ getPositionalNodeRefs ref
    nodeMays <- mapM (zoom Graph.tcAST . runASTOp . ASTBuilder.getNodeId) args
    let withInd  = zip nodeMays [0..]
        onlyExt  = catMaybes $ (\(n, i) -> (,) <$> n <*> Just i) <$> withInd
        conns    = flip fmap onlyExt $ \(n, i) -> (OutPortRef n All, InPortRef nodeId (Arg i))
    return $ (maybeToList selfConnMay) ++ conns
