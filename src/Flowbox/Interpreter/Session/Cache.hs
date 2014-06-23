---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.Cache where

import qualified Data.List                    as List
import qualified GHC
import           Language.Haskell.Interpreter as I

import           Flowbox.Control.Error
import           Flowbox.Interpreter.Mockup.Graph    (CodeGraph)
import qualified Flowbox.Interpreter.Mockup.Graph    as Graph
import           Flowbox.Interpreter.Session.Session (Session)
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.Cache"


dump :: Graph.ID -> Session ()
dump nodeID = do
    logger trace $ "Dumping " ++ show nodeID
    void $ I.runGhc $ GHC.runStmt ("print " ++ argPrefix ++ show nodeID) GHC.RunToCompletion


invalidate :: Graph.ID -> Session ()
invalidate nodeID = do
    logger trace $ "Invalidating " ++ show nodeID
    void $ I.runGhc $ GHC.runStmt (argPrefix ++ show nodeID ++ " <- return ()") GHC.RunToCompletion


runNode :: Graph.ID -> CodeGraph -> Session ()
runNode nodeID graph = do
    node <- Graph.lab graph nodeID <?> "No node with id=" ++ show nodeID
    let
        predecessors = Graph.pre graph nodeID
        args         = map (\i -> argPrefix ++ show i) predecessors
        functionMod  = "return $ extract $ "
        function     = functionMod ++ " (Operation " ++ node ++ ")"
        argSeparator = " `call` "
        operation    = List.intercalate argSeparator (function : args)
        expression   = argPrefix ++ show nodeID ++ " <- " ++ operation
    logger trace expression
    void $ I.runGhc $ GHC.runStmt expression GHC.RunToCompletion


argPrefix :: String
argPrefix = "_"
