module Empire.Commands.GraphBuilder where

import           Prologue
import           Control.Monad.State
import           Control.Monad.Error     (throwError)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import qualified Data.IntMap             as IntMap
import           Data.Maybe              (catMaybes, maybeToList, fromMaybe)
import qualified Data.Text.Lazy          as Text

import           Data.Variants           (match, case', ANY(..))
import           Data.Layer.Coat         (uncoat, coated)

import qualified Empire.Data.Graph       as Graph
import           Empire.Data.Graph       (Graph)

import           Empire.API.Data.Port     (InPort(..), OutPort(..))
import           Empire.API.Data.PortRef  (InPortRef(..), OutPortRef(..))
import           Empire.API.Data.Node     (NodeId)
import           Empire.API.Data.NodeMeta (NodeMeta(..))
import qualified Empire.API.Data.Node     as API
import qualified Empire.API.Data.Graph    as API

import           Empire.Empire
import qualified Empire.Commands.AST        as AST
import           Empire.Commands.AST        (ASTOp)
import qualified Empire.Commands.GraphUtils as GraphUtils

import qualified Luna.Syntax.Builder        as Builder
import           Luna.Syntax.Repr.Graph     (Ref, Node, Edge)
import           Luna.Syntax.AST.Term       (Var(..), App(..), Blank(..), Accessor(..), Unify(..), Draft, Val)
import qualified Luna.Syntax.AST.Lit        as Lit

type VarMap = Map (Ref Node) NodeId

buildGraph :: Command Graph API.Graph
buildGraph = API.Graph <$> buildNodes <*> buildConnections

buildNodes :: Command Graph [API.Node]
buildNodes = do
    allNodeIds <- uses Graph.nodeMapping IntMap.keys
    forM allNodeIds $ \id -> do
        ref  <- GraphUtils.getASTTarget id
        uref <- GraphUtils.getASTPointer id
        expr <- zoom Graph.ast $ AST.runAstOp $ getNodeExpression ref
        meta <- zoom Graph.ast $ AST.readMeta uref
        return $ API.Node id (Text.pack expr) Map.empty $ fromMaybe def meta

buildConnections :: Command Graph [(OutPortRef, InPortRef)]
buildConnections = do
    varMap <- getVarMap
    allNodes <- uses Graph.nodeMapping IntMap.keys
    edges <- mapM (getNodeInputs varMap) allNodes
    return $ concat edges

getVarMap :: Command Graph VarMap
getVarMap = do
    allNodes <- uses Graph.nodeMapping IntMap.keys
    vars <- mapM GraphUtils.getASTVar allNodes
    return $ Map.fromList $ zip vars allNodes

getNodeExpression :: Ref Node -> ASTOp String
getNodeExpression ref = do
    node <- Builder.readRef ref
    case' (uncoat node) $ do
        match $ \(Var n) -> Builder.follow n >>= AST.printIdent
        match $ \(Accessor n _) -> Builder.follow n >>= AST.printIdent
        match $ \(App f _) -> Builder.follow f >>= getNodeExpression
        match $ \val -> do
            case' (val :: Val (Ref Edge)) $ do
                match $ \(Lit.Int i)    -> return $ show i
                match $ \(Lit.String s) -> return $ show s

getSelfNodeRef :: Ref Node -> ASTOp (Maybe (Ref Node))
getSelfNodeRef nodeRef = do
    node <- Builder.readRef nodeRef
    case' (uncoat node) $ do
        match $ \(Accessor _ t) -> Builder.follow t >>= return . Just
        match $ \(App t _)      -> Builder.follow t >>= getSelfNodeRef
        match $ \ANY            -> return Nothing

getPositionalNodeRefs :: Ref Node -> ASTOp [Ref Node]
getPositionalNodeRefs nodeRef = do
    node <- Builder.readRef nodeRef
    case' (uncoat node) $ do
        match $ \(App _ args) -> AST.unpackArguments args
        match $ \ANY          -> return []

getNodeInputs :: VarMap -> NodeId -> Command Graph [(OutPortRef, InPortRef)]
getNodeInputs varMap nodeId = do
    ref     <- GraphUtils.getASTTarget nodeId
    selfMay <- zoom Graph.ast $ AST.runAstOp $ getSelfNodeRef ref
    let selfNodeMay = selfMay >>= flip Map.lookup varMap
        selfConnMay = (,) <$> (OutPortRef <$> selfNodeMay <*> Just All)
                          <*> (Just $ InPortRef nodeId Self)

    args <- zoom Graph.ast $ AST.runAstOp $ getPositionalNodeRefs ref
    let nodeMays = flip Map.lookup varMap <$> args
        withInd  = zip nodeMays [0..]
        onlyExt  = catMaybes $ (\(n, i) -> (,) <$> n <*> Just i) <$> withInd
        conns    = flip fmap onlyExt $ \(n, i) -> (OutPortRef n All, InPortRef nodeId (Arg i))
    return $ (maybeToList selfConnMay) ++ conns
