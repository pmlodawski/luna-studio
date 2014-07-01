---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.Cache where

import           Control.Monad.Trans.State
import qualified Data.List                 as List

import           Flowbox.Control.Error
import qualified Flowbox.Data.MapForest                as MapForest
import           Flowbox.Interpreter.Mockup.Graph      (CodeGraph)
import qualified Flowbox.Interpreter.Mockup.Node       as Node
import qualified Flowbox.Interpreter.Mockup.Type       as Type
import           Flowbox.Interpreter.Session.CallPath  (CallPath)
import qualified Flowbox.Interpreter.Session.CallPath  as CallPath
import           Flowbox.Interpreter.Session.CallPoint (CallPoint)
import qualified Flowbox.Interpreter.Session.Env       as Env
import           Flowbox.Interpreter.Session.Session   (Session)
import qualified Flowbox.Interpreter.Session.Session   as Session
import qualified Flowbox.Luna.Data.AST.Zipper.Focus    as Focus
import qualified Flowbox.Luna.Data.AST.Zipper.Zipper   as Zipper
import qualified Flowbox.Luna.Lib.LibManager           as LibManager
import qualified Flowbox.Luna.Lib.Library              as Library
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.Cache"





dump :: CallPath -> Session ()
dump callPath = do
    logger debug $ "Dumping " ++ show callPath
    Session.runStmt ("print " ++ argPrefix ++ show callPath)


invalidate :: CodeGraph -> CallPath -> Session ()
invalidate graph callPath = do
    modify (Env.cached %~ MapForest.delete callPath)
    logger debug $ "Invalidating " ++ show callPath
    Session.runStmt (CallPath.toVarName callPath ++ " <- return ()")
    --mapM_ (invalidate graph) $ Node.suc graph nodeID




--runNodeIfNeeded :: CodeGraph -> CallPath -> Session ()
--runNodeIfNeeded graph callPath = unlessM (isCached nodeID) (runNode graph nodeID)


--runNode :: CodeGraph -> CallPath -> Session ()
--runNode graph callPath = do
--    node <- Node.lab graph nodeID <?> "No node with id=" ++ show nodeID
--    let predecessors = Node.pre graph nodeID

--    mapM_ (runNodeIfNeeded graph) predecessors

--    let functionName = node ^. Node.code
--        functionType = node ^. Node.cls . Type.repr
--        args         = map (\i -> argPrefix ++ show i) predecessors
--        function     = "toIO $ extract $ (Operation (" ++ functionName ++ " :: " ++ functionType ++ "))"
--        argSeparator = " `call` "
--        operation    = List.intercalate argSeparator (function : args)
--        expression   = argPrefix ++ show nodeID ++ " <- " ++ operation
--    Session.runStmt expression
--    modify (Env.cached %~ MapForest.insert nodeID)
--    logger trace =<< show <$> gets (view Env.cached)


isCached :: CallPath -> Session Bool
isCached callPath = MapForest.member callPath <$> gets (view Env.cached)


argPrefix :: String
argPrefix = "_"


previous :: CallPath -> Session [CallPath]
previous callPath = do
    undefined

next :: CallPath -> Session [CallPath]
next callPath = do
    undefined

execute :: CallPath -> Session ()
execute callPath = do
    undefined


findMain :: Session CallPath
findMain = do
    libManager              <- gets (view Env.libManager)
    (mainLibraryID, mainBC) <- gets (view Env.mainPtr)
    mainLibrary             <- LibManager.lab libManager mainLibraryID
                            <??> "Main library not found"

    let ast   = mainLibrary ^. Library.ast

    mainFocus <- Zipper.getFocus <$> Zipper.focusBreadcrumbs' mainBC ast
    mainExpr  <- Focus.getFunction mainFocus <??> "Cannot find main"

    print mainExpr
    return undefined
