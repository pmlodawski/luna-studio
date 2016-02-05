---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Batch.Handler.Common where

import           Control.Monad.RWS
import           Data.Int                                                        (Int32)
import qualified Data.List                                                       as List
import           Text.Show.Pretty

import           Flowbox.Batch.Batch                                             (Batch)
import qualified Flowbox.Batch.Batch                                             as Batch
import           Flowbox.Batch.Project.Project                                   (Project)
import qualified Flowbox.Batch.Project.Project                                   as Project
import           Flowbox.Batch.Project.ProjectManager                            (ProjectManager)
import qualified Flowbox.Batch.Project.ProjectManager                            as ProjectManager
import           Flowbox.Control.Error
import           Flowbox.Prelude                                                 hiding (cons, error)
import           Flowbox.System.Log.Logger
import qualified Luna.DEP.AST.AST                                                as AST
import           Luna.DEP.AST.Control.Crumb                                      (Breadcrumbs)
import           Luna.DEP.AST.Control.Focus                                      (Focus)
import qualified Luna.DEP.AST.Control.Focus                                      as Focus
import qualified Luna.DEP.AST.Control.Zipper                                     as Zipper
import           Luna.DEP.AST.Expr                                               (Expr)
import qualified Luna.DEP.AST.Expr                                               as Expr
import           Luna.DEP.AST.Module                                             (Module)
import           Luna.DEP.Data.ASTInfo                                           (ASTInfo)
import           Luna.DEP.Graph.Graph                                            (Graph)
import qualified Luna.DEP.Graph.Graph                                            as Graph
import           Luna.DEP.Graph.Node                                             (Node)
import qualified Luna.DEP.Graph.Node                                             as Node
import           Luna.DEP.Graph.PropertyMap                                      (PropertyMap)
import qualified Luna.DEP.Graph.PropertyMap                                      as PropertyMap
import           Luna.DEP.Graph.View.Default.DefaultsMap                         (DefaultsMap)
import qualified Luna.DEP.Graph.View.Default.DefaultsMap                         as DefaultsMap
import           Luna.DEP.Graph.View.Default.Expr                                (DefaultExpr)
import           Luna.DEP.Graph.View.GraphView                                   (GraphView)
import qualified Luna.DEP.Graph.View.GraphView                                   as GraphView
import           Luna.DEP.Graph.View.PortDescriptor                              (PortDescriptor)
import           Luna.DEP.Lib.Lib                                                (Library)
import qualified Luna.DEP.Lib.Lib                                                as Library
import           Luna.DEP.Lib.Manager                                            (LibManager)
import qualified Luna.DEP.Lib.Manager                                            as LibManager
import qualified Luna.DEP.Pass.Analysis.Alias.Alias                              as Alias
import qualified Luna.DEP.Pass.Transform.AST.IDFixer.IDFixer                     as IDFixer
import qualified Luna.DEP.Pass.Transform.Graph.Builder.Builder                   as GraphBuilder
import qualified Luna.DEP.Pass.Transform.Graph.GCNodeProperties.GCNodeProperties as GCNodeProperties
import qualified Luna.DEP.Pass.Transform.Graph.Parser.Parser                     as GraphParser
import qualified Luna.DEP.Pass.Transform.GraphView.Defaults                      as Defaults
import qualified Luna.DEP.Pass.Transform.SimpleText.Builder.Builder              as STBuilder
import qualified Luna.DEP.Pass.Transform.SimpleText.Parser.Parser                as STParser



logger :: LoggerIO
logger = getLoggerIO $moduleName


increaseUpdateNo :: Batch ()
increaseUpdateNo = modify (Batch.updateNo %~ (+1))


getUpdateNo :: Batch Int32
getUpdateNo = gets (view Batch.updateNo)


setUpdateNo :: Int32 -> Batch ()
setUpdateNo updateNo = modify (set Batch.updateNo updateNo)


getProjectManager :: Batch ProjectManager
getProjectManager = gets (view Batch.projectManager)


setProjectManager :: ProjectManager -> Batch ()
setProjectManager projectManager =
    increaseUpdateNo >> modify (set Batch.projectManager projectManager)


getProject :: Project.ID -> Batch Project
getProject projectID = do
    projectManager <- getProjectManager
    ProjectManager.lab projectManager projectID <??> ("Wrong 'projectID' = " ++ show projectID)


setProject :: Project -> Project.ID -> Batch ()
setProject newProject projectID = do
    pm <- getProjectManager
    setProjectManager (ProjectManager.updateNode (projectID, newProject) pm)


getLibManager :: Project.ID -> Batch LibManager
getLibManager projectID = view Project.libs <$> getProject projectID


setLibManager :: LibManager -> Project.ID -> Batch ()
setLibManager newLibManager projectID = do
    project <- getProject projectID
    setProject (project & Project.libs .~ newLibManager) projectID


getLibrary :: Library.ID -> Project.ID -> Batch Library
getLibrary libraryID projectID = do
    libManager <- getLibManager projectID
    LibManager.lab libManager libraryID <??> ("Wrong 'libraryID' = " ++ show libraryID)


setLibrary :: Library -> Library.ID -> Project.ID -> Batch ()
setLibrary newLibary libraryID projectID = do
    libManager <- getLibManager projectID
    setLibManager (LibManager.updateNode (libraryID, newLibary) libManager) projectID


getASTInfo :: Library.ID -> Project.ID -> Batch ASTInfo
getASTInfo = fmap (view Library.astInfo) .: getLibrary


setASTInfo :: ASTInfo -> Library.ID -> Project.ID -> Batch ()
setASTInfo astInfo libraryID projectID = do
    library <- getLibrary libraryID projectID
    setLibrary (library & Library.astInfo .~ astInfo) libraryID projectID


getPropertyMap :: Library.ID -> Project.ID -> Batch PropertyMap
getPropertyMap libraryID projectID =
    view Library.propertyMap <$> getLibrary libraryID projectID


setPropertyMap :: PropertyMap -> Library.ID -> Project.ID -> Batch ()
setPropertyMap newPropertyMap libraryID projectID = do
    library <- getLibrary libraryID projectID
    setLibrary (library & Library.propertyMap .~ newPropertyMap) libraryID projectID


getDefaultsMap :: Node.ID -> Library.ID -> Project.ID -> Batch DefaultsMap
getDefaultsMap nodeID libraryID projectID =
    PropertyMap.getDefaultsMap nodeID <$> getPropertyMap libraryID projectID


lookupNodeDefault :: PortDescriptor -> Node.ID -> Library.ID -> Project.ID
                  -> Batch (Maybe DefaultExpr)
lookupNodeDefault inPort nodeID libraryID projectID =
    DefaultsMap.lookup inPort <$> getDefaultsMap nodeID libraryID projectID


getAST :: Library.ID -> Project.ID -> Batch  Module
getAST libraryID projectID =
    view Library.ast <$> getLibrary libraryID projectID


setAST :: Module -> Library.ID -> Project.ID -> Batch ()
setAST newModule libraryID projectID = do
    library <- getLibrary libraryID projectID
    setLibrary (library & Library.ast .~ newModule) libraryID projectID


getFocus :: Breadcrumbs -> Library.ID -> Project.ID -> Batch Focus
getFocus bc libraryID projectID = do
    m <- getAST libraryID projectID
    hoistEither $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' bc m


setFocus :: Focus -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setFocus newFocus bc libraryID projectID = do
    m      <- getAST libraryID projectID
    zipper <- hoistEither $ Zipper.focusBreadcrumbs' bc m
    let newM = Zipper.close $ Zipper.modify (const newFocus) zipper
    setAST newM libraryID projectID


getModule :: Breadcrumbs -> Library.ID -> Project.ID -> Batch Module
getModule bc libraryID projectID = do
    focus <- getFocus bc libraryID projectID
    Focus.getModule focus <??> "Target is not a module"


setModule :: Module -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setModule newModule = setFocus (Focus.Module newModule)


getFunction :: Breadcrumbs -> Library.ID -> Project.ID -> Batch Expr
getFunction bc libraryID projectID = do
    focus <- getFocus bc libraryID projectID
    Focus.getFunction focus <??> "Target is not a function"


setFunctionFocus :: Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setFunctionFocus newFunction = setFocus (Focus.Function newFunction)


getData :: Breadcrumbs -> Library.ID -> Project.ID -> Batch Expr
getData bc libraryID projectID = do
    focus <- getFocus bc libraryID projectID
    Focus.getClass focus <??> "Target is not a class"


setData :: Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setData newClass = setFocus (Focus.Class newClass)


getDataCon :: AST.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch Expr
getDataCon conID bc libraryID projectID = do
    data_ <- getData bc libraryID projectID
    List.find ((==) conID . view Expr.id) (data_ ^. Expr.cons) <??> "No data constructor with id = " ++ show conID


setDataCon :: Expr -> AST.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setDataCon con conID bc libraryID projectID = do
    data_ <- getData bc libraryID projectID
    let cons = data_ ^. Expr.cons
        (a, b) = List.break ((==) conID . view Expr.id) cons
    b' <- case b of
        _:t -> return $ con : t
        _   -> left $ "No data constructor with id = " ++ show conID
    setData (data_ & Expr.cons .~ (a ++ b')) bc libraryID projectID


getGraph :: Breadcrumbs -> Library.ID -> Project.ID -> Batch (Graph, PropertyMap)
getGraph bc libraryID projectID = do
    ast         <- getAST libraryID projectID
    logger trace $ ppShow ast
    propertyMap <- getPropertyMap libraryID projectID
    expr        <- getFunction bc libraryID projectID
    aa          <- EitherT $ Alias.run ast
    result <- EitherT $ GraphBuilder.run aa propertyMap True expr
    logger trace $ ppShow result
    return result


setGraph :: (Graph, PropertyMap) -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setGraph (newGraph, newPM) bc libraryID projectID = do
    logger trace $ ppShow newGraph
    logger trace $ ppShow newPM
    expr <- getFunction bc libraryID projectID
    (ast, newPM2)  <- EitherT $ GraphParser.run newGraph newPM expr
    astInfo <- getASTInfo libraryID projectID
    (fixedAst, astInfo') <- EitherT $ IDFixer.runExpr astInfo Nothing False ast
    setASTInfo astInfo' libraryID projectID
    logger debug $ show newGraph
    logger debug $ show newPM2
    logger debug $ ppShow fixedAst
    setFunctionFocus fixedAst bc libraryID projectID
    setPropertyMap newPM2 libraryID projectID
    gcPropertyMap libraryID projectID


getCode :: Breadcrumbs -> Library.ID -> Project.ID -> Batch String
getCode bc libraryID projectID = do
    expr <- getFunction bc libraryID projectID
    fst <$> EitherT (STBuilder.run def expr)


setCode :: String -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setCode code bc libraryID projectID = do
    expr      <- getFunction bc libraryID projectID
    newExpr   <- EitherT $ STParser.run code expr
    astInfo   <- getASTInfo libraryID projectID
    (fixedExpr, astInfo') <- EitherT $ IDFixer.runExpr astInfo Nothing True newExpr
    setASTInfo astInfo' libraryID projectID
    setFunctionFocus (Expr.id .~ (newExpr ^. Expr.id) $ fixedExpr) bc libraryID projectID


gcPropertyMap :: Library.ID -> Project.ID -> Batch ()
gcPropertyMap libraryID projectID = do
    ast <- getAST         libraryID projectID
    pm  <- getPropertyMap libraryID projectID
    fixedPM  <- EitherT $ GCNodeProperties.run ast pm
    setPropertyMap fixedPM libraryID projectID


getGraphView :: Breadcrumbs -> Library.ID -> Project.ID -> Batch (GraphView, PropertyMap)
getGraphView bc libraryID projectID = do
    (graph, propertyMap) <- getGraph bc libraryID projectID
    let (graphView, propertyMap2) = GraphView.fromGraph graph propertyMap
    return $ Defaults.removeDefaults graphView propertyMap2


setGraphView :: (GraphView, PropertyMap) -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setGraphView (newGraphView', newPM') bc libraryID projectID = do
    let (newGraphView, newPM) = Defaults.addDefaults newGraphView' newPM'
    graphWithPm <- hoistEither $ GraphView.toGraph newGraphView newPM
    setGraph graphWithPm bc libraryID projectID


getNode :: Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch Node
getNode nodeID bc libraryID projectID = do
    (graphView, _) <- getGraphView bc libraryID projectID
    Graph.lab graphView nodeID <??> ("Wrong 'nodeID' = " ++ show nodeID)


getNodes :: [Node.ID] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch [(Node.ID, Maybe Node)]
getNodes nodeIDs bc libraryID projectID = do
    (graphView, _) <- getGraphView bc libraryID projectID
    return $ map (\nodeID -> (nodeID, Graph.lab graphView nodeID)) nodeIDs

---------------------------------------------------------------------------

projectManagerOp :: (ProjectManager -> Batch (ProjectManager, r)) -> Batch r
projectManagerOp operation = do
    projectManager <- getProjectManager
    (newProjectManager, r) <- operation projectManager
    setProjectManager newProjectManager
    return r


projectOp :: Project.ID -> (Project -> Batch (Project, r)) -> Batch r
projectOp projectID operation = do
    project         <- getProject projectID
    (newProject, r) <- operation project
    setProject newProject projectID
    return r


libManagerOp :: Project.ID -> (LibManager -> Batch (LibManager, r)) -> Batch r
libManagerOp projectID operation = do
    libManager <- getLibManager projectID
    (newLibManager, r) <- operation libManager
    setLibManager newLibManager projectID
    return r


libraryOp :: Library.ID -> Project.ID
          -> (Library -> Batch (Library, r))
          -> Batch r
libraryOp libraryID projectID operation =  do
    library        <- getLibrary libraryID projectID
    (newLibary, r) <- operation library
    setLibrary newLibary libraryID projectID
    return r


propertyMapOp :: Library.ID -> Project.ID
              -> (PropertyMap -> Batch (PropertyMap, r))
              -> Batch r
propertyMapOp libraryID projectID operation = do
    propertyMap <- getPropertyMap libraryID projectID
    (newPropertyMap, r) <- operation propertyMap
    setPropertyMap newPropertyMap libraryID projectID
    return r


astOp :: Library.ID -> Project.ID
      -> (Module -> PropertyMap -> Batch ((Module, PropertyMap), r))
      -> Batch r
astOp libraryID projectID operation = do
    ast         <- getAST         libraryID projectID
    propertyMap <- getPropertyMap libraryID projectID
    ((newAst, newPM), r) <- operation ast propertyMap
    setAST         newAst libraryID projectID
    setPropertyMap newPM  libraryID projectID
    return r


astFocusOp :: Breadcrumbs -> Library.ID -> Project.ID
           -> (Focus -> Batch (Focus, r))
           -> Batch r
astFocusOp bc libraryID projectID operation = do
    focus <- getFocus bc libraryID projectID
    (newFocus, r) <- operation focus
    setFocus newFocus bc libraryID projectID
    return r


astModuleOp :: Breadcrumbs -> Library.ID -> Project.ID
            -> (Module -> Batch (Module, r))
            -> Batch r
astModuleOp bc libraryID projectID operation = do
    m <- getModule bc libraryID projectID
    (newM, r) <- operation m
    setModule newM bc libraryID projectID
    return r


astFunctionOp :: Breadcrumbs -> Library.ID -> Project.ID
              -> (Expr -> Batch (Expr, r))
              -> Batch r
astFunctionOp bc libraryID projectID operation = do
    f <- getFunction bc libraryID projectID
    (newF, r) <- operation f
    setFunctionFocus newF bc libraryID projectID
    return r


astDataOp :: Breadcrumbs -> Library.ID -> Project.ID
          -> (Expr -> Batch (Expr, r)) -> Batch r
astDataOp bc libraryID projectID operation = do
    c <- getData bc libraryID projectID
    (newC, r) <- operation c
    setData newC bc libraryID projectID
    return r


astDataConOp :: AST.ID -> Breadcrumbs -> Library.ID -> Project.ID
             -> (Expr -> Batch (Expr, r)) -> Batch r
astDataConOp conID bc libraryID projectID operation = do
    c <- getDataCon conID bc libraryID projectID
    (c', r) <- operation c
    setDataCon c' conID bc libraryID projectID
    return r


graphOp :: Breadcrumbs -> Library.ID -> Project.ID
        -> (Graph -> PropertyMap -> Batch ((Graph, PropertyMap), r))
        -> Batch r
graphOp bc libraryID projectID operation = do
    (graph, pm)            <- getGraph bc libraryID projectID
    ((newGraph, newPM), r) <- operation graph pm
    setGraph (newGraph, newPM) bc libraryID projectID
    return r


graphViewOp :: Breadcrumbs -> Library.ID -> Project.ID
            -> (GraphView -> PropertyMap -> Batch ((GraphView, PropertyMap), r))
            -> Batch r
graphViewOp bc libraryID projectID operation = do
    (graphView, propertyMap) <- getGraphView bc libraryID projectID
    ((newGraphView, newPM), r) <- operation graphView propertyMap
    setGraphView (newGraphView, newPM) bc libraryID projectID
    return r
