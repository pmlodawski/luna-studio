---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.Cache where

import           Control.Monad.Trans.State
import qualified Data.List                 as List
import qualified Data.Set                  as Set
import Control.Monad (filterM)
import           Flowbox.Control.Error
import           Flowbox.Interpreter.Mockup.Graph    (CodeGraph)
import qualified Flowbox.Interpreter.Mockup.Graph    as Graph
import qualified Flowbox.Interpreter.Session.Env     as Env
import           Flowbox.Interpreter.Session.Session (Session)
import qualified Flowbox.Interpreter.Session.Session as Session
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.Cache"


dump :: Graph.ID -> Session ()
dump nodeID = do
    logger trace $ "Dumping " ++ show nodeID
    Session.runStmt ("print " ++ argPrefix ++ show nodeID)


invalidate :: Graph.ID -> Session ()
invalidate nodeID = do
    modify (Env.cached %~ Set.delete nodeID)
    logger trace $ "Invalidating " ++ show nodeID
    Session.runStmt (argPrefix ++ show nodeID ++ " <- return ()")


runNode :: Graph.ID -> CodeGraph -> Session ()
runNode nodeID graph = do
    node <- Graph.lab graph nodeID <?> "No node with id=" ++ show nodeID
    let predecessors = Graph.pre graph nodeID
    
    needsRecache <- filterM (fmap not . isCached) predecessors
    mapM_ (flip runNode graph) needsRecache

    let args         = map (\i -> argPrefix ++ show i) predecessors
        functionMod  = "return $ extract $ "
        function     = functionMod ++ " (Operation " ++ node ++ ")"
        argSeparator = " `call` "
        operation    = List.intercalate argSeparator (function : args)
        expression   = argPrefix ++ show nodeID ++ " <- " ++ operation
    logger trace expression
    Session.runStmt expression
    modify (Env.cached %~ Set.insert nodeID)
    print =<< get


isCached :: Graph.ID -> Session Bool
isCached nodeID = Set.member nodeID <$> gets (view Env.cached)


argPrefix :: String
argPrefix = "_"
