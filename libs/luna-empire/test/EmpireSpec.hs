{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module EmpireSpec (spec) where

import           Prologue
import           Control.Exception (bracket)
import           Control.Monad.Except (throwError)
import           Data.List (find, sort)
import qualified Data.Map as Map
import           Data.UUID (nil)
import           Data.UUID.V4 (nextRandom)
import           Empire.API.Data.DefaultValue (PortDefault (..), Value (..))
import qualified Empire.API.Data.Graph        as Graph
import qualified Empire.Commands.GraphBuilder as GraphBuilder
import qualified Empire.Commands.GraphUtils   as GraphUtils
import           Empire.API.Data.Input        as Input
import           Empire.API.Data.Output       as Output
import           Empire.API.Data.Node         as Node
import           Empire.API.Data.NodeMeta
import           Empire.API.Data.Port
import           Empire.API.Data.GraphLocation
import           Empire.API.Data.Breadcrumb   as Breadcrumb
import           Empire.API.Data.PortRef      (AnyPortRef(..), InPortRef (..), OutPortRef (..))
import           Empire.API.Data.TypeRep
import           Empire.API.Data.ValueType
import           Empire.Commands.AST
import           Empire.Commands.Graph        as Graph
import           Empire.Commands.Library
import           Empire.Commands.Project
import           Empire.Commands.Typecheck    as Typecheck
import           Empire.Data.AST
import           Empire.Data.Graph
import           Empire.Data.Library
import           Empire.Empire
import           Control.Concurrent.STM.TChan (TChan, newTChan)
import           Control.Concurrent.STM       (atomically)
import           Luna.Pretty.GraphViz         (renderAndOpen, toGraphViz)

import           Test.Hspec (around, describe, expectationFailure, it,
                             shouldBe, shouldSatisfy, shouldMatchList)


runGraph env act = runEmpire env def $ do
    (pid, _) <- createProject Nothing "dupa"
    (lid, _) <- createLibrary pid (Just "xd") "/xd/xd"
    let toLoc = GraphLocation pid lid
    act toLoc

-- runGraph' :: CommunicationEnv -> Env -> ((Breadcrumb -> GraphLocation) -> Command Graph a) -> IO (Either Error a, Env)
runGraph' env graph act = runEmpire env graph $ do
    pid <- (fst . head) <$> listProjects
    lid <- (fst . head) <$> listLibraries pid
    let toLoc = GraphLocation pid lid
    act toLoc

-- runGraph'' :: _ -> _ -> Graph -> _ -> _
runGraph'' env st newGraph act = runEmpire env st $ do
    pid <- (fst . head) <$> listProjects
    lid <- (fst . head) <$> listLibraries pid
    withLibrary pid lid $ body .= newGraph
    let toLoc = GraphLocation pid lid
    act toLoc

graphIDs loc = do
  nodes <- view Graph.nodes <$> Graph.getGraph loc
  let ids = map (^. nodeId) nodes
  return ids

exGraph (InterpreterEnv _ _ _ g _) = g

spec = around withChannels $
    describe "luna-empire" $ do
        it "descends into `def foo` and asserts null list of nodes inside" $ \env -> do
            u1 <- nextRandom
            (res, state) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                let loc' = mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                n1 <- Graph.addNode loc u1 "def foo" def
                topLevel <- graphIDs loc
                n1Level <- graphIDs loc'
                return (topLevel, n1Level)
            case res of
                Left err -> expectationFailure err
                Right ids -> u1 `shouldSatisfy` (`elem` (fst ids))
        it "adds node" $ \env -> do
            u1 <- nextRandom
            (res, _) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                n1 <- Graph.addNode loc u1 "def foo" def
                return n1
            case res of
                Left err -> expectationFailure err
                Right _ -> return ()
        it "has nodes inside function" $ \env -> do
            u1 <- nextRandom
            (res, _) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                n1 <- Graph.addNode loc u1 "def foo" def
                graphIDs $ mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
            case res of
                Left err -> expectationFailure err
                Right ids -> ids `shouldSatisfy` ((== 2) . length)
        it "int literal has no nodes inside" $ \env -> do
            u1 <- nextRandom
            (res, _) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                    loc' = mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                n1 <- Graph.addNode loc u1 "4" def
                graphIDs loc'
            case res of
                Left err -> expectationFailure err
                Right ids -> ids `shouldSatisfy` null
        it "properly typechecks input nodes" $ \env -> do
            u1 <- nextRandom
            (res, st) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                    loc' = mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                Graph.addNode loc u1 "-> $a $b a + b" def
                let GraphLocation pid lid _ = loc
                withLibrary pid lid (use $ body)
            case res of
                Left err -> expectationFailure err
                Right g -> do
                    (res', (exGraph -> g')) <- runEmpire env (InterpreterEnv def def def g def) $
                        Typecheck.run $ GraphLocation nil 0 $ Breadcrumb []
                    (Right foo,_) <- runGraph'' env st g' $ \mkLoc -> do
                        Graph.getGraph $ mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                    let nodes' = foo ^. Graph.nodes
                        Just input = find ((== "inputEdge") . Node._name) nodes'
                        inputsTypes = input ^. nodeType . inputs
                        types = map Input._valueType inputsTypes
                    types `shouldMatchList` [TypeIdent (TCons "Int" []), TypeIdent (TCons "Int" [])]
        it "properly typechecks output nodes" $ \env -> do
            u1 <- nextRandom
            (res, st) <- runGraph env $ \mkLoc -> do
                let loc = mkLoc $ Breadcrumb []
                    loc' = mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                Graph.addNode loc u1 "-> $a $b a + b" def
                let GraphLocation pid lid _ = loc
                withLibrary pid lid (use $ body)
            case res of
                Left err -> expectationFailure err
                Right g -> do
                    (res', (exGraph -> g')) <- runEmpire env (InterpreterEnv def def def g def) $
                        Typecheck.run $ GraphLocation nil 0 $ Breadcrumb []
                    (Right foo,_) <- runGraph'' env st g' $ \mkLoc -> do
                        Graph.getGraph $ mkLoc $ Breadcrumb [Breadcrumb.Lambda u1]
                    let nodes' = foo ^. Graph.nodes
                        Just output' = find ((== "outputEdge") . Node._name) nodes'
                        outputType = output' ^. nodeType
                        types = Output._valueType $ Node._output outputType
                    types `shouldBe` TypeIdent (TCons "Int" [])

withChannels :: (CommunicationEnv -> IO ()) -> IO ()
withChannels = bracket createChannels (const $ return ())
    where
        createChannels = atomically $ CommunicationEnv <$> newTChan <*> newTChan

test :: Empire Graph
test = do
    (pid, _) <- createProject Nothing "dupa"
    (lid, _) <- createLibrary pid (Just "xd") "/xd/xd"

    let loc = GraphLocation pid lid $ Breadcrumb []

    u1 <- liftIO $ nextRandom
    u2 <- liftIO $ nextRandom
    u3 <- liftIO $ nextRandom
    n1 <- Graph.addNode loc u1 "foo" def
    n2 <- Graph.addNode loc u2 "bar" def
    n3 <- Graph.addNode loc u3 "baz" def

    let loc' = GraphLocation pid lid $ Breadcrumb [Breadcrumb.Lambda u1]

    u4 <- liftIO $ nextRandom
    n4 <- Graph.addNode loc' u4 "cokolwiek" def

    let graphIDs loc = do
            nodes <- view Graph.nodes <$> Graph.getGraph loc
            let ids = map (^. nodeId) nodes
            return ids

    ids <- graphIDs loc
    when (u1 `notElem` ids || u2 `notElem` ids || u3 `notElem` ids) $ throwError "ERROR"

    ids <- graphIDs loc'
    when (u4 `notElem` ids) $ throwError "ERROR2"
    withLibrary pid lid (use $ body)

main :: IO ()
main = do
    dummyNotif <- atomically newTChan
    dummyTC    <- atomically newTChan
    (g, st)    <- runEmpire (CommunicationEnv dummyNotif dummyTC) def test
    case g of
        Left err -> putStrLn err
        Right g  -> do
            res <- runEmpire (CommunicationEnv dummyNotif dummyTC) (InterpreterEnv def def def g def) $ Typecheck.run $ GraphLocation nil 0 $ Breadcrumb []
            return ()
