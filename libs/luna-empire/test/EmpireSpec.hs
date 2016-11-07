{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module EmpireSpec (spec) where

import           Prologue
import           Control.Monad.Except (throwError)
import           Data.List (sort)
import           Data.UUID (nil)
import           Data.UUID.V4 (nextRandom)
import           Empire.API.Data.DefaultValue (PortDefault (..), Value (..))
import qualified Empire.API.Data.Graph        as Graph
import           Empire.API.Data.Node
import           Empire.API.Data.NodeMeta
import           Empire.API.Data.Port
import           Empire.API.Data.GraphLocation
import           Empire.API.Data.Breadcrumb   as Breadcrumb
import           Empire.API.Data.PortRef      (AnyPortRef(..), InPortRef (..), OutPortRef (..))
import           Empire.Commands.AST
import           Empire.Commands.Graph        as Graph
import           Empire.Commands.Library
import           Empire.Commands.Project
import           Empire.Commands.Typecheck    as Typecheck
import           Empire.Data.AST
import           Empire.Data.Graph
import           Empire.Data.Library
import           Empire.Empire
import           Control.Concurrent.STM.TChan (newTChan)
import           Control.Concurrent.STM       (atomically)
import           Luna.Pretty.GraphViz         (renderAndOpen, toGraphViz)

import           Test.Hspec (describe, it)


spec = describe "luna-empire" $ do
    it "works" $
        main

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
    -- mapM (const $ Graph.addNode loc "1.+ 1" def) [1..100]


    {-n1 <- Graph.addNode loc "3" def-}
    {-n2 <- Graph.addNode loc "6" def-}
    {-np <- Graph.addNode loc "_.+ _" def-}
    {-Graph.connect loc (OutPortRef n1 All) (InPortRef np Self)-}
    {-Graph.connect loc (OutPortRef n2 All) (InPortRef np $ Arg 0)-}

    {-print "tutti"-}
    {-n1 <- (view nodeId) <$> Graph.addNode pid lid "1"     (NodeMeta (1.0, 4.0))-}
    {-print "tutti"-}
    {-n2 <- (view nodeId) <$> Graph.addNode pid lid "2"     (NodeMeta (2.0, 3.0))-}
    {-print "tutti"-}
    {-np <- (view nodeId) <$> Graph.addNode pid lid "_.+ _"     (NodeMeta (3.0, 2.0))-}
    {-nt <- (view nodeId) <$> Graph.addNode pid lid "_.toString" def-}
    {-print "tutti"-}
    {-Graph.connect pid lid (OutPortRef n1 All) (InPortRef np Self)-}
    {-Graph.connect pid lid (OutPortRef n2 All) (InPortRef np (Arg 0))-}
    {-Graph.connect pid lid (OutPortRef n1 All) (InPortRef nt Self)-}
    {-print "tutti"-}
    {-Graph.renameNode pid lid n1 "dupcia"-}


    npi <- (view nodeId) <$> Graph.addNode loc nil "zero2pi" def
    nvs <- (view nodeId) <$> Graph.addNode loc nil "vsin _" def
    Graph.connect loc (OutPortRef npi All) (InPortRef nvs (Arg 0))

    {-nr1 <- (view nodeId) <$> Graph.addNode pid lid "range 0 10" def-}
    {-nr2 <- (view nodeId) <$> Graph.addNode pid lid "_.toDouble" def-}
    {-nr3 <- (view nodeId) <$> Graph.addNode pid lid "10.toDouble" def-}
    {-nr4 <- (view nodeId) <$> Graph.addNode pid lid "_./ _" def-}
    {-nr5 <- (view nodeId) <$> Graph.addNode pid lid "vsin _" def-}

    {-Graph.connect pid lid (OutPortRef nr1 All) (InPortRef nr2 Self)-}
    {-Graph.connect pid lid (OutPortRef nr2 All) (InPortRef nr4 Self)-}
    {-Graph.connect pid lid (OutPortRef nr3 All) (InPortRef nr4 (Arg 0))-}
    {-Graph.connect pid lid (OutPortRef nr2 All) (InPortRef nr3 (Arg 1))-}
    {-Graph.connect pid lid (OutPortRef nr4 All) (InPortRef nr5 (Arg 0))-}


    {-fstNode <- (view nodeId) <$> Graph.addNode pid lid "+"   (NodeMeta (3.14, 3.14))-}
    {-sndNode <- (view nodeId) <$> Graph.addNode pid lid "Int" (NodeMeta (4.14, 4.14))-}
    {-trdNode <- (view nodeId) <$> Graph.addNode pid lid "2"   (NodeMeta (5.14, 5.14))-}
    {-rthNode <- (view nodeId) <$> Graph.addNode pid lid "5"   (NodeMeta (6.14, 6.14))-}
    {-Graph.connect pid lid (OutPortRef rthNode All) (InPortRef fstNode (Arg 1))-}
    {-Graph.connect pid lid (OutPortRef trdNode All) (InPortRef fstNode (Arg 0))-}
    {-Graph.connect pid lid (OutPortRef sndNode All) (InPortRef fstNode Self)-}

    {-Graph.removeNode pid lid fstNode-}

    {-Graph.removeNode pid lid np-}

    {-Graph.disconnect pid lid (InPortRef fstNode Self)-}
    {-Graph.disconnect pid lid (InPortRef fstNode $ Arg 0)-}
    {-Graph.disconnect pid lid (InPortRef fstNode $ Arg 1)-}

    {-Graph.addNode pid lid "1.floor.toString" $ NodeMeta (0.0, 0.0)-}

    {-rn  <- view nodeId <$> Graph.addNode pid lid "+ 7 34" (NodeMeta (3.3, 4.4))-}
    {-rn1 <- view nodeId <$> Graph.addNode pid lid "7.* 4" (NodeMeta (3.3, 4.4))-}

    {-nn <- view nodeId <$> Graph.addNode pid lid "13.+ _" def-}
    {-nn2 <- view nodeId <$> Graph.addNode pid lid "_.+ 17" def-}
    {-Graph.setDefaultValue pid lid (InPortRef nn (Arg 0)) (Constant $ IntValue 0)-}
    {-Graph.setDefaultValue pid lid (InPortRef nn2 Self) (Constant $ IntValue 14)-}
    {-putStrLn "NOW RUNNING"-}
    {-putStrLn "------------------------"-}

    {-runGraph loc >>= print-}

    {-putStrLn "------------------------"-}
    {-putStrLn "RUN DONE"-}

    {-code <- Graph.getCode loc-}
    {-putStrLn code-}

    {-graph <- Graph.getGraph loc-}
    {-print graph-}

    withLibrary pid lid (use $ body)

main :: IO ()
main = do
    dummyNotif <- atomically newTChan
    dummyTC    <- atomically newTChan
    (g, st)    <- runEmpire (CommunicationEnv dummyNotif dummyTC) def test
    {-(g2, st2)  <- runEmpire (CommunicationEnv dummyNotif dummyTC) def -}
    {-print g2-}
    case g of
        Left err -> putStrLn err
        Right g  -> do
            res <- runEmpire (CommunicationEnv dummyNotif dummyTC) (InterpreterEnv def def def g def) $ Typecheck.run $ GraphLocation nil 0 $ Breadcrumb []
            return ()
            {-print res-}
{-main = return ()-}
