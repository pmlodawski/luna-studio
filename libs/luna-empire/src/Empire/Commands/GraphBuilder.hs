{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Empire.Commands.GraphBuilder where

import           Prologue

import           Control.Monad.Except              (throwError)
import           Control.Monad.State
import           Control.Monad.Trans.Maybe         (MaybeT (..), runMaybeT)

import           Data.Graph                        (source)
import qualified Data.IntMap                       as IntMap
import           Data.Layer_OLD.Cover_OLD          (covered, uncover)
import qualified Data.List                         as List
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, fromMaybe, maybeToList)
import           Data.Prop                         (prop)
import           Data.Record                       (ANY (..), caseTest, of')
import qualified Data.Text.Lazy                    as Text
import qualified Data.Tree                         as Tree
import qualified Data.UUID                         as UUID
import qualified Data.UUID.V4                      as UUID (nextRandom)

import           Empire.API.Data.Breadcrumb        as Breadcrumb
import           Empire.Data.BreadcrumbHierarchy   (topLevelIDs)
import           Empire.Data.Graph                 (Graph)
import qualified Empire.Data.Graph                 as Graph

import           Empire.API.Data.DefaultValue      (PortDefault (..), Value (..))
import qualified Empire.API.Data.Graph             as API
import           Empire.API.Data.Node              (NodeId)
import qualified Empire.API.Data.Node              as API
import           Empire.API.Data.NodeMeta          (NodeMeta (..))
import           Empire.API.Data.Port              (InPort (..), OutPort (..), Port (..), PortId (..), PortState (..))
import qualified Empire.API.Data.Port              as Port
import           Empire.API.Data.PortRef           (InPortRef (..), OutPortRef (..))
import           Empire.API.Data.TypeRep           (TypeRep(TLam))
import           Empire.API.Data.ValueType         (ValueType (..))
import qualified Empire.API.Data.ValueType         as ValueType

import           Empire.ASTOp                      (ASTOp, runASTOp)
import qualified Empire.ASTOps.Builder             as ASTBuilder
import qualified Empire.ASTOps.Print               as Print
import qualified Empire.Commands.AST               as AST
import qualified Empire.Commands.GraphUtils        as GraphUtils
import           Empire.Data.AST                   (ClusRef, EdgeRef, NodeRef)
import           Empire.Empire

import qualified Luna.Syntax.Term.Function         as Function
import           Old.Luna.Syntax.Term.Class        (Acc (..), App (..), Blank (..), Cons (..), Lam (..), Match (..), Var (..))
import qualified Old.Luna.Syntax.Term.Expr.Lit     as Lit

import           Luna.Syntax.Model.Network.Builder (TCData (..), Type (..), replacement)
import qualified Luna.Syntax.Model.Network.Builder as Builder


decodeBreadcrumbs :: Breadcrumb BreadcrumbItem -> Command Graph (Breadcrumb (Named BreadcrumbItem))
decodeBreadcrumbs (Breadcrumb items) = do
    fmap Breadcrumb $ forM items $ \item@(Breadcrumb.Lambda nid) -> flip Named item <$> getNodeName nid

buildGraph :: Command Graph API.Graph
buildGraph = API.Graph <$> buildNodes <*> buildConnections

buildNodes :: Command Graph [API.Node]
buildNodes = do
    allNodeIds <- uses Graph.breadcrumbHierarchy topLevelIDs
    edges <- buildEdgeNodes
    nodes <- mapM buildNode allNodeIds
    return $ nodes ++ case edges of
        Just (inputEdge, outputEdge) -> [inputEdge, outputEdge]
        _                            -> []

type EdgeNodes = (API.Node, API.Node)

buildEdgeNodes :: Command Graph (Maybe EdgeNodes)
buildEdgeNodes = getEdgePortMapping >>= \p -> case p of
    Just (inputPort, outputPort) -> do
        inputEdge <- buildInputEdge inputPort
        outputEdge <- buildOutputEdge outputPort
        return $ Just (inputEdge, outputEdge)
    _ -> return Nothing

getOrCreatePortMapping :: NodeId -> Command Graph (NodeId, NodeId)
getOrCreatePortMapping nid = do
    existingMapping <- uses Graph.breadcrumbPortMapping $ Map.lookup nid
    case existingMapping of
        Just m -> return m
        _      -> do
            ids <- liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom
            Graph.breadcrumbPortMapping . at nid ?= ids
            return ids

getEdgePortMapping :: Command Graph (Maybe (NodeId, NodeId))
getEdgePortMapping = do
    lastBreadcrumbId <- use Graph.insideNode
    case lastBreadcrumbId of
        Just id -> do
            isLambda <- rhsIsLambda id
            if isLambda
                then Just <$> getOrCreatePortMapping id
                else return Nothing
        _ -> return Nothing

buildNode :: NodeId -> Command Graph API.Node
buildNode nid = do
    ref   <- GraphUtils.getASTTarget nid
    uref  <- GraphUtils.getASTPointer nid
    expr  <- zoom Graph.ast $ runASTOp $ Print.printNodeExpression ref
    meta  <- zoom Graph.ast $ AST.readMeta uref
    name  <- getNodeName nid
    canEnter <- rhsIsLambda nid
    ports <- buildPorts ref
    let code    = Nothing -- Just $ Text.pack expr
        portMap = Map.fromList $ flip fmap ports $ \p@(Port id _ _ _) -> (id, p)
    return $ API.Node nid (Text.pack name) (API.ExpressionNode $ Text.pack expr) canEnter portMap (fromMaybe def meta) code

rhsIsLambda :: NodeId -> Command Graph Bool
rhsIsLambda nid = do
    ref <- GraphUtils.getASTTarget nid
    zoom Graph.ast $ runASTOp $ do
        node <- Builder.read ref
        caseTest (uncover node) $ do
            of' $ \(Lam _ _) -> return True
            of' $ \ANY       -> return False

getNodeName :: NodeId -> Command Graph String
getNodeName nid = do
    vref <- GraphUtils.getASTVar nid
    zoom Graph.ast $ runASTOp $ do
        vnode <- Builder.read vref
        caseTest (uncover vnode) $ of' $ \(Var (Lit.String n)) -> return . toString $ n

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
                Lit.Double   d -> DoubleValue   d
            of' $ \(Cons (Lit.String s) _) -> case s of
                "False" -> return . WithDefault . Constant . BoolValue $ False
                "True"  -> return . WithDefault . Constant . BoolValue $ True
                _       -> WithDefault . Expression <$> Print.printExpression ref
            of' $ \Blank   -> return NotConnected
            of' $ \ANY     -> WithDefault . Expression <$> Print.printExpression ref

extractAppArgTypes :: ASTOp m => NodeRef -> m [ValueType]
extractAppArgTypes ref = do
    node <- Builder.read ref
    args <- runMaybeT $ do
        repl :: ClusRef <- MaybeT $ cast <$> Builder.follow (prop TCData . replacement) ref
        sig <- MaybeT $ Builder.follow (prop Builder.Lambda) repl
        return $ sig ^. Function.args
    case args of
        Nothing -> return []
        Just as -> do
            let unpacked = unlayer <$> as
            types <- mapM (Builder.follow (prop Type) >=> Builder.follow source) unpacked
            mapM getTypeRep types

extractArgTypes :: ASTOp m => NodeRef -> m [ValueType]
extractArgTypes ref = do
    node <- Builder.read ref
    caseTest (uncover node) $ do
        of' $ \(Lam args out) -> do
            unpacked <- ASTBuilder.unpackArguments args
            as     <- mapM getTypeRep unpacked
            tailAs <- Builder.follow source out >>= extractArgTypes
            return $ as ++ tailAs
        of' $ \ANY -> return []

extractPortInfo :: ASTOp m => NodeRef -> m ([ValueType], [PortState])
extractPortInfo ref = do
    node  <- Builder.read ref
    caseTest (uncover node) $ do
        of' $ \(App f args) -> do
            unpacked       <- ASTBuilder.unpackArguments args
            portStates     <- mapM getPortState unpacked
            tp    <- Builder.follow source f >>= Builder.follow (prop Type) >>= Builder.follow source
            types <- extractArgTypes tp
            return (types, portStates)
        of' $ \(Lam as o) -> do
            args     <- ASTBuilder.unpackArguments as
            areBlank <- mapM ASTBuilder.isBlank args
            isApp    <- ASTBuilder.isApp =<< Builder.follow source o
            if and areBlank && isApp
                then extractPortInfo =<< Builder.follow source o
                else do
                    tpRef <- Builder.follow source $ node ^. prop Type
                    types <- extractArgTypes tpRef
                    return (types, [])
        of' $ \ANY -> do
            tpRef <- Builder.follow source $ node ^. prop Type
            types <- extractArgTypes tpRef
            return (types, [])

buildArgPorts :: ASTOp m => NodeRef -> m [Port]
buildArgPorts ref = do
    (types, states) <- extractPortInfo ref
    let psCons = zipWith3 Port (InPortId . Arg <$> [0..]) (("arg " <>) . show <$> [0..]) (types ++ replicate (length states - length types) AnyType)
    return $ zipWith ($) psCons (states ++ repeat NotConnected)

buildSelfPort' :: ASTOp m => Bool -> NodeRef -> m (Maybe Port)
buildSelfPort' seenAcc nodeRef = do
    node <- Builder.read nodeRef
    let buildPort noType = do
            tpRep     <- if noType then return AnyType else followTypeRep nodeRef
            portState <- getPortState nodeRef
            return . Just $ Port (InPortId Self) "self" tpRep portState

    caseTest (uncover node) $ do
        of' $ \(Acc _ t)  -> Builder.follow source t >>= buildSelfPort' True
        of' $ \(App t _)  -> Builder.follow source t >>= buildSelfPort' seenAcc
        of' $ \(Lam as o) -> do
            args <- ASTBuilder.unpackArguments as
            areBlank <- mapM ASTBuilder.isBlank args
            if and areBlank
                then Builder.follow source o >>= buildSelfPort' seenAcc
                else if seenAcc then buildPort False else return Nothing
        of' $ \Blank      -> return Nothing
        of' $ \(Var _)    -> if seenAcc then buildPort False else buildPort True
        of' $ \ANY        -> if seenAcc then buildPort False else return Nothing

buildSelfPort :: ASTOp m => NodeRef -> m (Maybe Port)
buildSelfPort = buildSelfPort' False

followTypeRep :: ASTOp m => NodeRef -> m ValueType
followTypeRep ref = do
    tp <- Builder.follow source =<< Builder.follow (prop Type) ref
    getTypeRep tp

getTypeRep :: ASTOp m => NodeRef -> m ValueType
getTypeRep tp = do
    rep <- Print.getTypeRep tp
    return $ TypeIdent rep

buildPorts :: NodeRef -> Command Graph [Port]
buildPorts ref = zoom Graph.ast $ runASTOp $ do
    selfPort <- maybeToList <$> buildSelfPort ref
    argPorts <- buildArgPorts ref
    tpRep    <- followTypeRep ref
    outState <- getPortState ref
    return $ selfPort ++ argPorts ++ [Port (OutPortId All) "Output" tpRep outState]

buildConnections :: Command Graph [(OutPortRef, InPortRef)]
buildConnections = do
    allNodes <- uses Graph.breadcrumbHierarchy topLevelIDs
    edges <- getEdgePortMapping
    connections <- mapM (getNodeInputs edges) allNodes
    outputEdgeConnections <- forM edges $ uncurry getOutputEdgeInputs
    let foo = maybeToList outputEdgeConnections
    return $ concat foo ++ concat connections

buildInputEdge :: NodeId -> Command Graph API.Node
buildInputEdge nid = do
    lastb <- use Graph.insideNode <?!> "top-level nodes have no input edge"
    ref   <- GraphUtils.getASTTarget lastb
    (types, states) <- zoom Graph.ast $ runASTOp $ extractPortInfo ref
    argTypes <- case types of
        [] -> do
            numberOfArguments <- length <$> (zoom Graph.ast $ runASTOp $ extractArgTypes ref)
            return $ replicate numberOfArguments AnyType
        _ -> return types
    out <- zoom Graph.ast $ runASTOp $ followTypeRep ref
    let nameGen = fmap (\i -> Text.pack $ "input" ++ show i) [0..]
        inputEdges = zipWith3 (\n t i -> Port (OutPortId $ Projection i) (Text.unpack n) t Port.NotConnected) nameGen argTypes [0..]
    return $
        API.Node nid
            "inputEdge"
            API.InputEdge
            False
            (Map.fromList $ flip map inputEdges $ \port -> (port ^. Port.portId, port))
            def
            def

buildOutputEdge :: NodeId -> Command Graph API.Node
buildOutputEdge nid = do
    lastb <- use Graph.insideNode <?!> "top-level nodes have no output edge"
    ref <- GraphUtils.getASTTarget lastb
    out <- zoom Graph.ast $ runASTOp $ followTypeRep ref
    outputType <- case out of
        TypeIdent (TLam _ t) -> return $ TypeIdent t
        TypeIdent t -> return $ TypeIdent t
        a -> return a
    let port = Port (InPortId $ Arg 0) "output" outputType Port.NotConnected
    return $
        API.Node nid
            "outputEdge"
            API.OutputEdge
            False
            (Map.fromList [(port ^. Port.portId, port)])
            def
            def

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

getLambdaNodeRefs :: ASTOp m => NodeRef -> m NodeRef
getLambdaNodeRefs nodeRef = do
    node <- Builder.read nodeRef
    caseTest (uncover node) $ do
        of' $ \(Lam _ out) -> Builder.follow source out

getLambdaArgRefs :: ASTOp m => NodeRef -> m [NodeRef]
getLambdaArgRefs nodeRef = do
    node <- Builder.read nodeRef
    caseTest (uncover node) $ do
        of' $ \(Lam args _) -> ASTBuilder.unpackArguments args
        of' $ \ANY          -> return []

getOutputEdgeInputs :: NodeId -> NodeId -> Command Graph [(OutPortRef, InPortRef)]
getOutputEdgeInputs inputEdge outputEdge = do
    lambda <- use Graph.insideNode <?!> "top-level nodes have no edges"
    ref <- GraphUtils.getASTTarget lambda
    output <- zoom Graph.ast $ runASTOp $ getLambdaNodeRefs ref
    nid <- zoom Graph.ast $ runASTOp $ ASTBuilder.getNodeId output
    case nid of
        Just id -> do
            let connectionToOutputEdge = (OutPortRef id All, InPortRef outputEdge (Arg 0))
            return [connectionToOutputEdge]
        _ -> return []

resolveInputNodeId :: Maybe (NodeId, NodeId) -> [NodeRef] -> NodeRef -> Command Graph (Maybe NodeId)
resolveInputNodeId edgeNodes lambdaArgs ref = do
    nodeId <- zoom Graph.ast $ runASTOp $ ASTBuilder.getNodeId ref
    case nodeId of
        Just nid -> return $ Just nid
        _ -> case List.find (== ref) lambdaArgs of
            Just a -> case edgeNodes of
                Just (i, _) -> return $ Just i
                _ -> return Nothing
            _ -> return Nothing

getOuterLambdaArguments :: Command Graph [NodeRef]
getOuterLambdaArguments = do
    lambda <- use Graph.insideNode
    case lambda of
        Just lambda -> do
            ref <- GraphUtils.getASTTarget lambda
            lambdaArgs <- zoom Graph.ast $ runASTOp $ getLambdaArgRefs ref
            return lambdaArgs
        _ -> return []

getNodeInputs :: Maybe (NodeId, NodeId) -> NodeId -> Command Graph [(OutPortRef, InPortRef)]
getNodeInputs edgeNodes nodeId = do
    ref         <- GraphUtils.getASTTarget nodeId
    selfMay     <- zoom Graph.ast $ runASTOp $ getSelfNodeRef ref
    selfNodeMay <- case selfMay of
        Just self -> zoom Graph.ast $ runASTOp $ ASTBuilder.getNodeId self
        Nothing   -> return Nothing
    let selfConnMay = (,) <$> (OutPortRef <$> selfNodeMay <*> Just All)
                          <*> (Just $ InPortRef nodeId Self)

    args       <- zoom Graph.ast $ runASTOp $ getPositionalNodeRefs ref
    lambdaArgs <- getOuterLambdaArguments
    nodeMays   <- mapM (resolveInputNodeId edgeNodes lambdaArgs) args
    let withInd  = zip nodeMays [0..]
        onlyExt  = catMaybes $ (\(n, i) -> (,) <$> n <*> Just i) <$> withInd
        conns    = flip fmap onlyExt $ \(n, i) -> (OutPortRef n All, InPortRef nodeId (Arg i))
    return $ maybeToList selfConnMay ++ conns
