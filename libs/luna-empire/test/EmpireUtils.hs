{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module EmpireUtils
    ( (|>-)
    , (|>)
    , (|>=)
    , addNode
    , connectToInput
    , emptyCodeTemplate
    , emptyGraphLocation
    , evalEmp
    , extractGraph
    , findNodeByName
    , findNodeIdByName
    , graphIDs
    , inPortRef
    , mkAliasPort
    , mkAllPort
    , mkSelfPort
    , mkUUID
    , noCheck
    , normalizeQQ
    , outPortRef
    , runEmp
    , runEmp'
    , runEmpire
    , runTests
    , specifyCodeChange
    , testCase
    , testCaseWithMarkers
    , top
    , withChannels
    , withResult
    , xitWithReason
    ) where

import           Control.Concurrent.MVar         (newEmptyMVar)
import           Control.Concurrent.STM          (atomically)
import           Control.Concurrent.STM.TChan    (newTChan)
import           Control.Exception               (bracket)
import           Control.Lens                    (uses)
import           Data.Char                       (isSpace)
import           Data.Coerce                     (coerce)
import           Data.List                       (dropWhileEnd)
import qualified Data.Map                        as Map
import           Data.Reflection                 (Given (..), give)
import qualified Data.Text                       as Text
import           Data.UUID                       (UUID)
import           Data.UUID.V4                    (nextRandom)
import           Empire.ASTOp                    (runASTOp)
import qualified Empire.Commands.Graph           as Graph
import qualified Empire.Commands.Graph           as Graph (addNode, connect, getNodes, loadCode)
import           Empire.Commands.Library         (createLibrary, listLibraries, withLibrary)
import qualified Empire.Commands.Library         as Library
import           Empire.Data.AST                 ()
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Graph               (CommandState(..), ClsGraph, defaultPMState, userState)
import qualified Empire.Data.Graph               as Graph
import qualified Empire.Data.Library             as Library (body, path)
import           Empire.Empire                   (CommunicationEnv (..), Empire, Env, InterpreterEnv (..), runEmpire)
import           Empire.Prelude                  hiding (toList)
import           LunaStudio.Data.Breadcrumb      (Breadcrumb (..), BreadcrumbItem (Arg, Definition, Lambda))
import           LunaStudio.Data.Connection      (Connection)
import           LunaStudio.Data.GraphLocation   (GraphLocation (..))
import           LunaStudio.Data.Node            (ExpressionNode, NodeId, nodeId)
import qualified LunaStudio.Data.Node            as Node
import           LunaStudio.Data.NodeLoc         (NodeLoc (..))
import           LunaStudio.Data.NodeMeta        (NodeMeta (..))
import           LunaStudio.Data.Port            (InPort, InPortIndex(Self), OutPort, Port (Port), PortState)
import qualified LunaStudio.Data.Port            as Port
import           LunaStudio.Data.PortRef         (AnyPortRef(InPortRef'), InPortRef(..), OutPortRef(..))
import qualified LunaStudio.Data.Position        as Position
import           LunaStudio.Data.TypeRep         (TypeRep (TStar))
import           Test.Hspec                      (Arg, Example, Expectation, Spec, SpecWith
                                                 , around, before_, describe, it, parallel, pendingWith, shouldBe)
import           Text.RawString.QQ               (r)


runEmp :: CommunicationEnv -> (Given GraphLocation => Empire a) -> IO (a, CommandState Env)
runEmp env act = do
    pm <- defaultPMState
    runEmpire env (CommandState pm def) $ do
        _ <- createLibrary (Just "/TestFile") "/TestFile"
        let toLoc = GraphLocation "/TestFile"
        Graph.loadCode (toLoc (Breadcrumb [])) "def main:\n    None"
        [node] <- Graph.getNodes (toLoc (Breadcrumb []))
        -- uuid <- mkUUID
        -- Graph.addNode (toLoc $ Breadcrumb []) uuid "def main" def
        -- [node] <- Graph.getNodes (toLoc (Breadcrumb []))
        give (toLoc $ Breadcrumb [Definition (node ^. Node.nodeId)]) act

evalEmp :: CommunicationEnv -> (Given GraphLocation => Empire a) -> IO a
evalEmp env act = fst <$> runEmp env act

runEmp' :: CommunicationEnv -> CommandState Env -> ClsGraph ->
              (Given GraphLocation => Empire a) -> IO (a, CommandState Env)
runEmp' env st newGraph act = runEmpire env st $ do
    lib <- head <$> listLibraries
    let path = lib ^. Library.path
    withLibrary path $ userState . Library.body .= newGraph
    let toLoc = GraphLocation path
    give (toLoc $ Breadcrumb []) act

graphIDs :: GraphLocation -> Empire [NodeId]
graphIDs loc = do
    nodes <- Graph.getNodes loc
    let ids = fmap (^. nodeId) nodes
    return ids

extractGraph :: CommandState InterpreterEnv -> ClsGraph
extractGraph = _clsGraph . view userState

withResult :: a -> (a -> IO b) -> IO b
withResult res act = act res

top :: Given GraphLocation => GraphLocation
top = given

infixl 5 |>
(|>) :: GraphLocation -> NodeId -> GraphLocation
(|>) (GraphLocation file bc) nid = GraphLocation file $ coerce $ (<> [Lambda nid]) $ coerce bc

infixl 5 |>-
(|>-) :: GraphLocation -> (NodeId, Int) -> GraphLocation
(|>-) (GraphLocation file bc) it = GraphLocation file $ Breadcrumb $ (<> [uncurry Arg it]) $ coerce bc

infixl 5 |>=
(|>=) :: GraphLocation -> NodeId -> GraphLocation
(|>=) (GraphLocation file bc) it = GraphLocation file $ Breadcrumb $ (<> [Definition it]) $ coerce bc

withChannels :: (CommunicationEnv -> IO a) -> IO a
withChannels = bracket createChannels (const $ return ())
    where
        createChannels = CommunicationEnv <$> atomically newTChan <*> newEmptyMVar <*> newEmptyMVar

emptyGraphLocation :: GraphLocation
emptyGraphLocation = GraphLocation "" $ Breadcrumb []

mkUUID :: MonadIO m => m UUID
mkUUID = liftIO nextRandom

connectToInput :: GraphLocation -> OutPortRef -> InPortRef -> Empire Connection
connectToInput loc outPort inPort = Graph.connect loc outPort (InPortRef' inPort)

outPortRef :: NodeId -> Port.OutPortId -> OutPortRef
outPortRef = OutPortRef . NodeLoc def

inPortRef :: NodeId -> Port.InPortId -> InPortRef
inPortRef = InPortRef . NodeLoc def

mkAllPort :: Text -> PortState -> OutPort
mkAllPort name = Port mempty name TStar

mkSelfPort :: PortState -> InPort
mkSelfPort = Port [Self] "self" TStar

mkAliasPort :: PortState -> InPort
mkAliasPort = Port mempty "alias" TStar


addNode :: GraphLocation -> Text -> Empire ExpressionNode
addNode gl code = mkUUID >>= \nid -> Graph.addNode gl nid code def

findNodeIdByName :: GraphLocation -> Maybe Text -> Empire (Maybe NodeId)
findNodeIdByName = fmap2 (view Node.nodeId) .: findNodeByName

findNodeByName :: GraphLocation -> Maybe Text -> Empire (Maybe ExpressionNode)
findNodeByName gl name = findNode <$> Graph.getNodes gl where
    filterNodes = filter (\n -> n ^. Node.name == name)
    findNode nodes = case filterNodes nodes of
        [n] -> Just n
        _   -> Nothing


emptyCodeTemplate :: Text
emptyCodeTemplate = [r|
import Std.Base

def main:
    None
|]

normalizeQQ :: Text -> Text
normalizeQQ str = Text.intercalate "\n" $ Text.drop minWs <$> allLines where
    trimTrailingSpaces = Text.dropWhileEnd isSpace
    trimEmptyLines     = dropWhileEnd Text.null . dropWhile Text.null
    indentLength       = Text.length . Text.takeWhile isSpace
    allLines = trimEmptyLines $ trimTrailingSpaces <$> Text.lines str
    minWs    = minimum $ indentLength <$> filter (not . Text.null) allLines

codeCheck :: Text -> (Text -> Expectation)
codeCheck expectedCode = \resultCode -> 
    Text.strip resultCode `shouldBe` normalizeQQ expectedCode

noCheck :: a -> Expectation
noCheck _ = pure ()
    
mockNodesLayout :: GraphLocation -> Empire ()
mockNodesLayout gl = getMarkersWithNodeId >>= mapM_ (uncurry mockNodeMeta) where
    getMarkersWithNodeId = Graph.withGraph gl $ do
        markers <- fmap fromIntegral . Map.keys 
            <$> use (Graph.userState . Graph.codeMarkers)
        foundNodeIds <- runASTOp $ fmap catMaybes . forM markers $ \m ->
            (m,) `fmap2` Graph.getNodeIdForMarker m
        topLevelNodeIds <- uses
            (Graph.userState . Graph.breadcrumbHierarchy)
            BH.topLevelIDs
        pure $ filter (\(_, nid) -> elem nid topLevelNodeIds) foundNodeIds    
    mockNodeMeta marker nid = Graph.setNodeMeta gl nid $ NodeMeta
        (Position.fromTuple (0, fromIntegral marker * 10))
        False
        mempty

        
-- this function exists for backwards compatibility, meant to be removed soon
specifyCodeChange :: Text -> Text -> (GraphLocation -> Empire a) -> CommunicationEnv -> Expectation
specifyCodeChange initialCode expectedCode action env =
    testCase initialCode expectedCode noCheck action env

testCase
    :: Text
    -> Text
    -> (a -> Expectation)
    -> (GraphLocation -> Empire a)
    -> CommunicationEnv
    -> Expectation
testCase initialCode expectedCode resultCheck action env = let
        filePath = "/TestPath"
        topGl    = GraphLocation filePath def
        isMain n = n ^. Node.name == Just "main"
        execute  = do
            Library.createLibrary Nothing filePath
            Graph.loadCode topGl $ normalizeQQ initialCode
            mainNode <- filter isMain <$> Graph.getNodes topGl
            gl <- case mainNode of
                []     -> pure topGl
                [main] -> do
                    let gl = topGl |>= main ^. Node.nodeId
                    mockNodesLayout gl
                    pure gl
            (,) <$> action gl <*> Graph.getCode gl
    in evalEmp env execute >>= \(result, resultCode) -> do
        codeCheck expectedCode resultCode
        resultCheck result
        
-- This function is copy paste of testCase and is meant to be removed soon, when markers are removed from Luna
testCaseWithMarkers
    :: Text
    -> Text
    -> (a -> Expectation)
    -> (GraphLocation -> Empire a)
    -> CommunicationEnv
    -> Expectation
testCaseWithMarkers initialCode expectedCode resultCheck action env = let
        filePath = "/TestPath"
        topGl    = GraphLocation filePath def
        isMain n = n ^. Node.name == Just "main"
        execute  = do
            Library.createLibrary Nothing filePath
            Graph.loadCode topGl $ normalizeQQ initialCode
            mainNode <- filter isMain <$> Graph.getNodes topGl
            gl <- case mainNode of
                []     -> pure topGl
                [main] -> do
                    let gl = topGl |>= main ^. Node.nodeId
                    mockNodesLayout gl
                    pure gl
            (,) <$> action gl <*> Graph.withGraph gl (use Graph.code)
    in evalEmp env execute >>= \(result, resultCode) -> do
        codeCheck expectedCode resultCode
        resultCheck result

runTests :: String -> SpecWith CommunicationEnv -> Spec
runTests = around withChannels . parallel .: describe

xitWithReason :: (HasCallStack, Example a) 
    => String -> String -> a -> SpecWith (Arg a)
xitWithReason label reason action 
    = before_ (pendingWith reason) $ it label action