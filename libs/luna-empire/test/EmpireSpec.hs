{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module EmpireSpec (spec) where

import           Prologue hiding (mapping, toList, (|>))
import           Control.Exception (Exception, bracket)
import           Data.Coerce (coerce)
import           Data.Container.Class (usedIxes)
import           Data.Foldable (toList)
import           Data.Graph.Model.Node (nodeStore)
import           Data.List (find, stripPrefix)
import qualified Data.Map as Map
import           Data.UUID (nil)
import           Data.UUID.V4 (nextRandom)
import qualified Empire.API.Data.Graph        as Graph
import qualified Empire.Commands.GraphBuilder as GraphBuilder
import qualified Empire.Commands.GraphUtils   as GraphUtils
import           Empire.API.Data.Node         as Node
import           Empire.API.Data.Port
import           Empire.API.Data.GraphLocation hiding (breadcrumb)
import           Empire.API.Data.Breadcrumb   as Breadcrumb hiding (breadcrumb)
import           Empire.API.Data.PortRef      (InPortRef (..), OutPortRef (..))
import           Empire.API.Data.TypeRep
import           Empire.API.Data.ValueType
import           Empire.ASTOp                 (runASTOp)
import           Empire.ASTOps.Print          (printNodeExpression)
import           Empire.Commands.Graph        as Graph
import           Empire.Commands.Library
import           Empire.Commands.Project
import           Empire.Commands.Typecheck    as Typecheck
import           Empire.Data.Graph
import           Empire.Data.Library
import           Empire.Empire
import           Control.Concurrent.STM.TChan (newTChan)
import           Control.Concurrent.STM       (atomically)

import           Test.Hspec (Spec, around, describe, expectationFailure, it,
                             shouldBe, shouldContain, shouldSatisfy, shouldMatchList)

runGraph :: CommunicationEnv -> ((?graphLoc :: GraphLocation) => Command Env a) -> IO (Either Error a, Env)
runGraph env act = runEmpire env def $ do
    (pid, _) <- createProject Nothing "project1"
    (lid, _) <- createLibrary pid (Just "lib1") "/libs/lib1"
    let toLoc = GraphLocation pid lid
    let ?graphLoc = toLoc $ Breadcrumb [] in act

runGraph' :: CommunicationEnv -> Env -> Graph ->
              ((?graphLoc :: GraphLocation) => Command Env a) -> IO (Either Error a, Env)
runGraph' env st newGraph act = runEmpire env st $ do
    pid <- (fst . head) <$> listProjects
    lid <- (fst . head) <$> listLibraries pid
    withLibrary pid lid $ body .= newGraph
    let toLoc = GraphLocation pid lid
    let ?graphLoc = toLoc $ Breadcrumb [] in act

graphIDs :: GraphLocation -> Command Env [NodeId]
graphIDs loc = do
  nodes <- view Graph.nodes <$> Graph.getGraph loc
  let ids = map (^. nodeId) nodes
  return ids

extractGraph :: InterpreterEnv -> Graph
extractGraph (InterpreterEnv _ _ _ g _) = g

excludeEdges :: [Node] -> [Node]
excludeEdges = filter (not . isEdge . view nodeType)
    where
        isEdge InputEdge{}  = True
        isEdge OutputEdge{} = True
        isEdge _            = False

data DummyB = DummyB deriving (Show, Exception)

-- DummyB is only here because expectationFailure returns IO () and we need to
-- return arbitrary type from lambda
withResult :: Either String a -> (a -> IO b) -> IO b
withResult (Left err)  _   = expectationFailure err >> throwM DummyB
withResult (Right res) act = act res

top :: (?graphLoc :: GraphLocation) => GraphLocation
top = ?graphLoc

infixr 5 |>
(|>) :: GraphLocation -> NodeId -> GraphLocation
(|>) (GraphLocation pid lid bc) nid = GraphLocation pid lid $ coerce $ (++ [Breadcrumb.Lambda nid]) $ coerce bc

spec :: Spec
spec = around withChannels $
    describe "luna-empire" $ do
        it "descends into `def foo` and asserts null list of nodes inside" $ \env -> do
            u1 <- nextRandom
            (res, _) <- runGraph env $ do
                Graph.addNode top u1 "def foo" def
                topLevel <- graphIDs top
                n1Level <- graphIDs (top |> u1)
                return (topLevel, n1Level)
            withResult res $ \ids -> do
                u1 `shouldSatisfy` (`elem` (fst ids))
        it "adds node" $ \env -> do
            u1 <- nextRandom
            (res, _) <- runGraph env $ do
                Graph.addNode top u1 "def foo" def
            withResult res $ \_ -> return ()
        it "makes connection to output edge" $ \env -> do
            u1 <- nextRandom
            u2 <- nextRandom
            (res, _) <- runGraph env $ do
                _ <- Graph.addNode top u1 "def foo" def
                let loc' = top |> u1
                n2 <- Graph.addNode loc' u2 "4" def
                Just (_, out) <- Graph.withGraph loc' $ GraphBuilder.getEdgePortMapping
                let referenceConnection = (OutPortRef (n2 ^. nodeId) All, InPortRef out (Arg 0))
                Graph.connect loc' (referenceConnection ^. _1) (referenceConnection ^. _2)
                connections <- view Graph.connections <$> Graph.getGraph loc'
                return (referenceConnection, connections)
            withResult res $ \(conn, connections) -> do
                connections `shouldSatisfy` ((== 1) . length)
                head connections `shouldBe` conn
        it "connects input edge to succ (Self)" $ \env -> do
            u1 <- nextRandom
            u2 <- nextRandom
            (res, _) <- runGraph env $ do
                Graph.addNode top u1 "def foo" def
                let loc' = top |> u1
                Graph.addNode loc' u2 "succ" def
                Just (input, _) <- Graph.withGraph loc' $ GraphBuilder.getEdgePortMapping
                let referenceConnection = (OutPortRef input All, InPortRef u2 Self)
                Graph.connect loc' (referenceConnection ^. _1) (referenceConnection ^. _2)
                connections <- view Graph.connections <$> Graph.getGraph loc'
                return (referenceConnection, connections)
            withResult res $ \(conn, connections) -> do
                connections `shouldSatisfy` ((== 2) . length)
                connections `shouldContain` [conn]
        it "connects input edge to dummy node (Arg 0)" $ \env -> do
            u1 <- nextRandom
            u2 <- nextRandom
            (res, _) <- runGraph env $ do
                let loc' = top |> u1
                Graph.addNode top u1 "def foo" def
                n2 <- Graph.addNode loc' u2 "succ" def
                Just (input, _) <- Graph.withGraph loc' $ GraphBuilder.getEdgePortMapping
                let referenceConnection = (OutPortRef input All, InPortRef (n2 ^. nodeId) (Arg 0))
                Graph.connect loc' (referenceConnection ^. _1) (referenceConnection ^. _2)
                connections <- view Graph.connections <$> Graph.getGraph loc'
                return (referenceConnection, connections)
            withResult res $ \(conn, connections) -> do
                connections `shouldSatisfy` ((== 2) . length)
                connections `shouldContain` [conn]
        it "has proper connection inside `def foo`" $ \env -> do
            u1 <- nextRandom
            (res, _) <- runGraph env $ do
                let loc' = top |> u1
                Graph.addNode top u1 "def foo" def
                conns <- view Graph.connections <$> Graph.getGraph loc'
                Just edges <- Graph.withGraph loc' $ GraphBuilder.getEdgePortMapping
                return (conns, edges)
            withResult res $ \(connections, (inputEdge, outputEdge)) -> do
                connections `shouldMatchList` [(OutPortRef inputEdge (Projection 0), InPortRef outputEdge (Arg 0))]
        it "shows connection inside lambda" $ \env -> do
            u1 <- nextRandom
            u2 <- nextRandom
            u3 <- nextRandom
            (res, _) <- runGraph env $ do
                let loc' = top |> u1
                Graph.addNode top u1 "def foo" def
                n2 <- Graph.addNode loc' u2 "4" def
                n3 <- Graph.addNode loc' u3 "succ" def
                let referenceConnection = (OutPortRef (n2 ^. nodeId) All, InPortRef (n3 ^. nodeId) Self)
                Graph.connect loc' (referenceConnection ^. _1) (referenceConnection ^. _2)
                connections <- view Graph.connections <$> Graph.getGraph loc'
                return (referenceConnection, connections)
            withResult res $ \(ref, connections) -> do
                connections `shouldSatisfy` ((== 2) . length)
                connections `shouldContain` [ref]
        it "creates two nested lambdas and a node inside" $ \env -> do
            u1 <- nextRandom
            u2 <- nextRandom
            u3 <- nextRandom
            (res, _) <- runGraph env $ do
                let loc' = top |> u1
                Graph.addNode top u1 "def foo" def
                Graph.addNode loc' u2 "def bar" def
                let loc'' = loc' |> u2
                Graph.addNode loc'' u3 "4" def
                graphIDs loc''
            withResult res $ \ids -> do
                u3 `shouldSatisfy` (`elem` ids)
                u1 `shouldSatisfy` (`notElem` ids)
                u2 `shouldSatisfy` (`notElem` ids)
        it "cannot enter lambda applied to value" $ \env -> do
            u1 <- nextRandom
            u2 <- nextRandom
            (res, _) <- runGraph env $ do
                Graph.addNode top u1 "def foo" def
                Graph.addNode top u2 "4" def
                Graph.connect top (OutPortRef u2 All) (InPortRef u1 (Arg 0))
                Graph.getGraph top
            withResult res $ \g -> do
                let Just lambdaNode = find ((== u1) . Node._nodeId) $ Graph._nodes g
                lambdaNode ^. Node.canEnter `shouldBe` False
        it "has no null node inside `def foo`" $ \env -> do
            u1 <- nextRandom
            (res, _) <- runGraph env $ do
                Graph.addNode top u1 "def foo" def
                view Graph.nodes <$> Graph.getGraph (top |> u1)
            withResult res $ \(excludeEdges -> ids) -> do
                ids `shouldSatisfy` null
        it "int literal has no nodes inside" $ \env -> do
            u1 <- nextRandom
            (res, _) <- runGraph env $ do
                Graph.addNode top u1 "4" def
                graphIDs $ top |> u1
            case res of
                Left err -> case stripPrefix "cannot enter node Just" err of
                    Just _ -> return ()
                    _      -> expectationFailure err
                Right _  -> expectationFailure "should throw"
        it "properly typechecks input nodes" $ \env -> do
            u1 <- nextRandom
            (res, st) <- runGraph env $ do
                Graph.addNode top u1 "-> $a $b a + b" def
                let GraphLocation pid lid _ = top
                withLibrary pid lid (use $ body)
            withResult res $ \g -> do
                (_, (extractGraph -> g')) <- runEmpire env (InterpreterEnv def def def g def) $
                    Typecheck.run $ GraphLocation nil 0 $ Breadcrumb []
                (res'', _) <- runGraph' env st g' $ do
                    Graph.getGraph $ top |> u1
                withResult res'' $ \foo -> do
                    let nodes' = foo ^. Graph.nodes
                        Just input = find ((== "inputEdge") . Node._name) nodes'
                        ports' = toList $ input ^. ports
                        types = map (view valueType) ports'
                    types `shouldMatchList` [TypeIdent (TCons "Int" []), TypeIdent (TCons "Int" [])]
        it "properly typechecks output nodes" $ \env -> do
            u1 <- nextRandom
            (res, st) <- runGraph env $ do
                Graph.addNode top u1 "-> $a $b a + b" def
                let GraphLocation pid lid _ = top
                withLibrary pid lid (use $ body)
            withResult res $ \g -> do
                (_, (extractGraph -> g')) <- runEmpire env (InterpreterEnv def def def g def) $
                    Typecheck.run $ GraphLocation nil 0 $ Breadcrumb []
                (res'',_) <- runGraph' env st g' $ do
                    Graph.getGraph $ top |> u1
                withResult res'' $ \foo -> do
                    let nodes' = foo ^. Graph.nodes
                        Just output' = find ((== "outputEdge") . Node._name) nodes'
                        ports' = toList $ output' ^. ports
                        types = map (view valueType) ports'
                    types `shouldBe` [TypeIdent (TCons "Int" [])]
        it "adds lambda nodeid to node mapping" $ \env -> do
            u1 <- nextRandom
            (res, _) <- runGraph env $ do
                Graph.addNode top u1 "-> $a $b a + b" def
                withGraph top $ use nodeMapping
            withResult res $ \(toList -> mapping) -> do
                let isLambdaNode n = case n of
                        AnonymousNode _ -> True
                        _               -> False
                    lambdaNodes = filter isLambdaNode mapping
                lambdaNodes `shouldSatisfy` (not . null)
        it "puts + inside plus lambda" $ \env -> do
            u1 <- nextRandom
            (res, _) <- runGraph env $ do
                let loc' = top |> u1
                Graph.addNode top u1 "-> $a $b a + b" def
                view Graph.nodes <$> Graph.getGraph loc'
            withResult res $ \(excludeEdges -> nodes) -> do
                nodes `shouldSatisfy` ((== 1) . length)
                head nodes `shouldSatisfy` (\a -> a ^. nodeType . expression == "a + b")
        it "places connections between + node and output" $ \env -> do
          u1 <- nextRandom
          (res, _) <- runGraph env $ do
              let loc' = top |> u1
              Graph.addNode top u1 "-> $a $b a + b" def
              view Graph.connections <$> Graph.getGraph loc'
          withResult res $ \conns -> do
              conns `shouldSatisfy` ((== 1) . length)
        it "cleans after removing `def foo` with `4` inside connected to output" $ \env -> do
            u1 <- nextRandom
            u2 <- nextRandom
            (res, _) <- runGraph env $ do
                let loc' = top |> u1
                Graph.addNode top u1 "def foo" def
                n2 <- Graph.addNode loc' u2 "4" def
                Just (_, out) <- Graph.withGraph loc' $ GraphBuilder.getEdgePortMapping
                let referenceConnection = (OutPortRef (n2 ^. nodeId) All, InPortRef out (Arg 0))
                Graph.connect loc' (referenceConnection ^. _1) (referenceConnection ^. _2)
                Graph.removeNodes top [u1]
                mapping <- withGraph top $ use nodeMapping
                endAst <- withGraph top $ use ast
                return (endAst, mapping)
            withResult res $ \(endAst, mapping) -> do
                mapping `shouldSatisfy` Map.null
                endAst ^. _Wrapped . nodeStore . to usedIxes `shouldMatchList` [0]
        it "ignores nodes outside lambda while pretty-printing code inside it" $ \env -> do
            u1 <- nextRandom
            u2 <- nextRandom
            u3 <- nextRandom
            u4 <- nextRandom
            u5 <- nextRandom
            (res, _) <- runGraph env $ do
                let loc' = top |> u1
                Graph.addNode top u1 "def foo" def
                Graph.addNode top u2 "3" def
                Graph.addNode top u3 "4" def
                Graph.addNode loc' u4 "5" def
                Graph.addNode loc' u5 "6" def
                (,) <$> Graph.getCode top <*> Graph.getCode loc'
            withResult res $ \(topCode, lambdaCode) -> do
                lines topCode `shouldMatchList` ["node3 = 4", "node2 = 3", "foo = -> $in0 in0"]
                lines lambdaCode `shouldMatchList` ["def foo in0:", "    node5 = 6", "    node4 = 5", "    in0"]
        it "properly pretty-prints functions" $ \env -> do
            u1 <- nextRandom
            (res, _) <- runGraph env $ do
                let loc' = top |> u1
                Graph.addNode top u1 "-> $a $b a + b" def
                Graph.getCode loc'
            withResult res $ \pprCode -> do
                    lines pprCode `shouldMatchList` ["def node1 a b:", "    a + b"]
        it "prints `def foo` function" $ \env -> do
          u1 <- nextRandom
          (res, _) <- runGraph env $ do
              let loc' = top |> u1
              Graph.addNode top u1 "def foo" def
              Graph.getCode loc'
          withResult res $ \pprCode -> do
                  lines pprCode `shouldMatchList` ["def foo in0:", "    in0"]
        it "prints node name for `def foo`" $ \env -> do
            u1 <- nextRandom
            (res, _) <- runGraph env $ do
                Graph.addNode top u1 "def foo" def
                target <- withGraph top $ GraphUtils.getASTTarget u1
                expr <- withGraph top $ zoom ast $ runASTOp $ printNodeExpression target
                return expr
            withResult res $ \expr -> expr `shouldBe` "-> $in0 in0"

withChannels :: (CommunicationEnv -> IO a) -> IO a
withChannels = bracket createChannels (const $ return ())
    where
        createChannels = atomically $ CommunicationEnv <$> newTChan <*> newTChan
