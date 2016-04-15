{-# LANGUAGE ScopedTypeVariables #-}

module Empire.Commands.GraphBuilder where

import           Prologue

import           Control.Monad.State
import           Control.Monad.Error          (throwError)
import           Control.Monad.Trans.Maybe    (MaybeT (..), runMaybeT)

import qualified Data.IntMap                  as IntMap
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes, fromMaybe, maybeToList)
import qualified Data.List                    as List
import qualified Data.Text.Lazy               as Text
import           Data.Record                  (ANY (..), caseTest, of')
import           Data.Layer_OLD.Cover_OLD (uncover, covered)
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

import           Empire.Data.AST              (NodeRef, EdgeRef, ClusRef)
import           Empire.ASTOp                 (ASTOp, runASTOp)
import qualified Empire.ASTOps.Builder        as ASTBuilder
import qualified Empire.ASTOps.Print          as Print
import qualified Empire.Commands.AST          as AST
import qualified Empire.Commands.GraphUtils   as GraphUtils
import           Empire.Empire

import           Old.Luna.Syntax.Term.Class       (Acc (..), App (..), Blank (..), Match (..), Var (..), Cons (..), Lam (..))
import qualified Old.Luna.Syntax.Term.Expr.Lit         as Lit
import qualified Luna.Syntax.Term.Function    as Function

import qualified Luna.Syntax.Model.Network.Builder  as Builder
import           Luna.Syntax.Model.Network.Builder  (Type (..), TCData (..), replacement, Lambda (..))


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
    expr  <- zoom Graph.ast $ runASTOp $ Print.printNodeExpression ref
    meta  <- zoom Graph.ast $ AST.readMeta uref
    name  <- getNodeName nid
    ports <- buildPorts ref
    let portMap = Map.fromList $ flip fmap ports $ \p@(Port id _ _ _) -> (id, p)
    return $ API.Node nid (Text.pack name) (API.ExpressionNode $ Text.pack expr) portMap $ fromMaybe def meta

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
        sig <- MaybeT $ Builder.follow (prop Lambda) repl
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
        of' $ \(Lam args _) -> do
            unpacked <- ASTBuilder.unpackArguments args
            mapM getTypeRep unpacked
        of' $ \ANY -> return []

buildArgPorts :: ASTOp m => NodeRef -> m [Port]
buildArgPorts ref = do
    node <- Builder.read ref
    (types, states) <- caseTest (uncover node) $ do
        of' $ \(App f args) -> do
            portTypes  <- extractAppArgTypes ref
            unpacked   <- ASTBuilder.unpackArguments args
            portStates <- mapM getPortState unpacked
            return (portTypes, portStates)
        of' $ \(Var _) -> do
            tpRef <- Builder.follow source $ node ^. prop Type
            types <- extractArgTypes tpRef
            return (types, [])
        of' $ \ANY -> return ([], [])
    let psCons = zipWith3 Port (InPortId . Arg <$> [0..]) (("arg " <>) . show <$> [0..]) types
    return $ zipWith ($) psCons (states ++ repeat NotConnected)

buildSelfPort' :: ASTOp m => Bool -> NodeRef -> m (Maybe Port)
buildSelfPort' seenAcc nodeRef = do
    node <- Builder.read nodeRef
    let buildPort = do
        tpRep     <- followTypeRep nodeRef
        portState <- getPortState nodeRef
        return . Just $ Port (InPortId Self) "self" tpRep portState

    caseTest (uncover node) $ do
        of' $ \(Acc _ t) -> Builder.follow source t >>= buildSelfPort' True
        of' $ \(App t _) -> Builder.follow source t >>= buildSelfPort' seenAcc
        of' $ \Blank     -> return Nothing
        of' $ \(Var _)   -> buildPort
        of' $ \ANY       -> if seenAcc then buildPort else return Nothing

buildSelfPort :: ASTOp m => NodeRef -> m (Maybe Port)
buildSelfPort = buildSelfPort' False

followTypeRep :: ASTOp m => NodeRef -> m ValueType
followTypeRep ref = do
    tp <- Builder.follow source =<< Builder.follow (prop Type) ref
    getTypeRep tp

getTypeString :: ASTOp m => Bool -> Bool -> NodeRef -> m String
getTypeString parenCons parenLam tp = do
    tpNode <- Builder.read tp
    let parenIf cond expr = if cond then "(" <> expr <> ")" else expr
    caseTest (uncover tpNode) $ do
        of' $ \(Cons (Lit.String s) as) -> do
            args <- ASTBuilder.unpackArguments as
            case s of
                "List" -> do
                    reps <- mapM (getTypeString False False) args
                    return $ "[" <> concat reps <> "..]"
                _ -> do
                    reps <- mapM (getTypeString True True) args
                    let shouldParen = parenCons && (not . null $ reps)
                    return $ parenIf shouldParen $ unwords (s : reps)
        of' $ \(Lam as out) -> do
            args   <- ASTBuilder.unpackArguments as
            outRep <- getTypeString False True =<< Builder.follow source out
            reps   <- mapM (getTypeString False True) args
            return $ parenIf parenLam $ intercalate " -> " reps <> " => " <> outRep
        of' $ \(Var (Lit.String n)) -> return $ List.delete '#' n
        of' $ \ANY -> return ""

getTypeRep :: ASTOp m => NodeRef -> m ValueType
getTypeRep tp = do
    str <- getTypeString False False tp
    case str of
        "" -> return AnyType
        s  -> return $ TypeIdent s

buildPorts :: NodeRef -> Command Graph [Port]
buildPorts ref = zoom Graph.ast $ runASTOp $ do
    selfPort <- maybeToList <$> buildSelfPort ref
    argPorts <- buildArgPorts ref
    tpRep    <- followTypeRep ref
    outState <- getPortState ref
    return $ selfPort ++ argPorts ++ [Port (OutPortId All) "Output" tpRep outState]

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
    selfMay     <- zoom Graph.ast $ runASTOp $ getSelfNodeRef ref
    selfNodeMay <- case selfMay of
        Just self -> zoom Graph.ast $ runASTOp $ ASTBuilder.getNodeId self
        Nothing   -> return Nothing
    let selfConnMay = (,) <$> (OutPortRef <$> selfNodeMay <*> Just All)
                          <*> (Just $ InPortRef nodeId Self)

    args <- zoom Graph.ast $ runASTOp $ getPositionalNodeRefs ref
    nodeMays <- mapM (zoom Graph.ast . runASTOp . ASTBuilder.getNodeId) args
    let withInd  = zip nodeMays [0..]
        onlyExt  = catMaybes $ (\(n, i) -> (,) <$> n <*> Just i) <$> withInd
        conns    = flip fmap onlyExt $ \(n, i) -> (OutPortRef n All, InPortRef nodeId (Arg i))
    return $ maybeToList selfConnMay ++ conns
