---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.Cache where

import           Control.Monad.Trans.State
import qualified Data.List                           as List
import qualified Data.Set                            as Set
import           Flowbox.Control.Error
import           Flowbox.Interpreter.Mockup.Graph    (CodeGraph)
import qualified Flowbox.Interpreter.Mockup.Node     as Node
import qualified Flowbox.Interpreter.Mockup.Type     as Type
import qualified Flowbox.Interpreter.Session.Env     as Env
import           Flowbox.Interpreter.Session.Session (Session)
import qualified Flowbox.Interpreter.Session.Session as Session
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.Cache"


dump :: Node.ID -> Session ()
dump nodeID = do
    logger trace $ "Dumping " ++ show nodeID
    Session.runStmt ("print " ++ argPrefix ++ show nodeID)


invalidate :: CodeGraph -> Node.ID -> Session ()
invalidate graph nodeID = do
    modify (Env.cached %~ Set.delete nodeID)
    logger trace $ "Invalidating " ++ show nodeID
    Session.runStmt (argPrefix ++ show nodeID ++ " <- return ()")
    mapM_ (invalidate graph) $ Node.suc graph nodeID


runNodeIfNeeded :: CodeGraph -> Node.ID -> Session ()
runNodeIfNeeded graph nodeID = unlessM (isCached nodeID) (runNode graph nodeID)


runNode :: CodeGraph -> Node.ID -> Session ()
runNode graph nodeID = do
    node <- Node.lab graph nodeID <?> "No node with id=" ++ show nodeID
    let predecessors = Node.pre graph nodeID

    mapM_ (runNodeIfNeeded graph) predecessors

    let functionName = node ^. Node.code
        functionType = node ^. Node.cls . Type.repr
        args         = map (\i -> argPrefix ++ show i) predecessors
        function     = "toIO $ extract $ (Operation (" ++ functionName ++ " :: " ++ functionType ++ "))"
        argSeparator = " `call` "
        operation    = List.intercalate argSeparator (function : args)
        expression   = argPrefix ++ show nodeID ++ " <- " ++ operation
    logger trace expression
    Session.runStmt expression
    modify (Env.cached %~ Set.insert nodeID)
    logger trace =<< show <$> get


isCached :: Node.ID -> Session Bool
isCached nodeID = Set.member nodeID <$> gets (view Env.cached)


argPrefix :: String
argPrefix = "_"
