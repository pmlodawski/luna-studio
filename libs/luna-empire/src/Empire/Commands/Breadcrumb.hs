{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE PartialTypeSignatures        #-}

module Empire.Commands.Breadcrumb where

import           Empire.Prelude

import           Control.Exception.Safe          (handle)
import           Control.Lens                    (Traversal', imap, (^?!))
import           Control.Monad                   (forM)
import           Control.Monad.Except            (throwError)
import           Control.Monad.Reader            (ask)
import           Control.Monad.State             (get, put)
import qualified Data.Bimap                      as Bimap
import           Data.Coerce                     (coerce)
import           Data.List                       (find)
import           Data.Maybe                      (listToMaybe, maybe)
import qualified Data.Map                        as Map
import qualified Data.UUID.V4                    as UUID

import           Empire.ASTOp                      (GraphOp, runASTOp, runAliasAnalysis)
import           Empire.ASTOps.BreadcrumbHierarchy as ASTBreadcrumb
import           Empire.ASTOps.Parse               as ASTParse
import           Empire.ASTOps.Read                as ASTRead
import           Empire.Commands.Code              (functionBlockStartRef, propagateLengths)
import           Empire.Commands.AST               as AST
import           Empire.Data.AST                   (NodeRef, astExceptionFromException, astExceptionToException)
import           Empire.Data.BreadcrumbHierarchy   (navigateTo, replaceAt)
import qualified Empire.Data.BreadcrumbHierarchy   as BH
import qualified Empire.Data.Graph                 as Graph
import           Empire.Data.Layers                (Marker, SpanLength)
import qualified Empire.Data.Library               as Library

import           LunaStudio.Data.Breadcrumb      (Breadcrumb (..), BreadcrumbItem (..), _Redirection)
import           LunaStudio.Data.Library         (LibraryId)
import           LunaStudio.Data.NodeLoc         (NodeLoc(..))
import           LunaStudio.Data.NodeId          (NodeId)
import           LunaStudio.Data.NodeCache       (portMappingMap)
import           LunaStudio.Data.PortRef         (OutPortRef(..))
import           LunaStudio.Data.Project         (ProjectId)
import qualified Luna.Package                    as Package
import qualified Luna.Syntax.Text.Parser.Ast.CodeSpan as CodeSpan
import           Data.Text.Span                  (SpacedSpan(..))

import           Empire.Commands.Library         (withLibrary)
import           Empire.Empire                   (Command, CommunicationEnv,
                                                  Empire, activeFiles, 
                                                  runEmpire, zoomCommand)
import qualified Path

import qualified Luna.IR              as IR
import qualified System.IO              as IO


makeGraph
    :: NodeRef -> Maybe NodeId-> Command Library.Library (NodeId, Graph.TopLevelGraph)
makeGraph fun lastUUID = zoomCommand Library.body $ makeGraphCls fun Nothing lastUUID

extractMarkers :: NodeRef -> GraphOp [Word64]
extractMarkers root = matchExpr root $ \case
    Marked m _ -> do
        marker <- ASTBreadcrumb.getMarker =<< source m
        pure [marker]
    _ -> do
        ins     <- inputs root
        markers <- mapM (\i -> source i >>= extractMarkers) ins
        pure $ concat markers

-- makeGraphCls :: NodeRef -> Maybe NodeId
--     -> Command Graph.ClsGraph (NodeId, Graph.Graph)
-- makeGraphCls fun lastUUID = do
--     nodeCache <- use $ Graph.userState . Graph.clsNodeCache
--     uuid      <- maybe (liftIO UUID.nextRandom) return lastUUID
--     (funName, ref, fileOffset) <- runASTOp $ do
--         putLayer @Marker fun . Just
--             =<< toPortMarker (OutPortRef (convert uuid) mempty)
--         asgFun <- ASTRead.cutThroughDocAndMarked fun
--         matchExpr asgFun $ \case
--             ASGFunction n _ _ -> do
--                 offset <- functionBlockStartRef asgFun
--                 name   <- handle (\(_e::ASTRead.InvalidNameException) -> pure "")
--                     $ ASTRead.getVarName' =<< source n
--                 pure (nameToString name, asgFun, offset)
--     let oldPortMapping = nodeCache ^. portMappingMap . at (uuid, Nothing)
--     portMapping <- fromJustM
--         (liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom)
--         oldPortMapping
--     globalMarkers <- use $ Graph.userState . Graph.clsCodeMarkers
--     let bh    = BH.LamItem portMapping ref def
--         graph = Graph.Graph bh def globalMarkers def def fileOffset nodeCache
--     Graph.userState . Graph.clsFuns . at uuid
--         ?= Graph.FunctionGraph funName graph Map.empty
--     updatedCache <- withRootedFunction uuid $ do
--         runASTOp $ do
--             markers <- extractMarkers ref
--             let localMarkers = Map.filterWithKey
--                     (\k _ -> k `elem` markers)
--                     globalMarkers
--             Graph.codeMarkers .= localMarkers
--             propagateLengths ref
--         runAliasAnalysis
--         runASTOp $ do
--             ASTBreadcrumb.makeTopBreadcrumbHierarchy ref
--             ASTBreadcrumb.restorePortMappings uuid (nodeCache ^. portMappingMap)
--             use Graph.graphNodeCache
--     Graph.userState . Graph.clsNodeCache .= updatedCache
--     pure (uuid, graph)

makeGraphCls :: NodeRef -> Maybe (String, NodeId) -> Maybe NodeId -> Command Graph.ClsGraph (NodeId, Graph.TopLevelGraph)
makeGraphCls fun className lastUUID = do
    isFun <- runASTOp $ do
        asgFun    <- ASTRead.cutThroughDocAndMarked fun
        matchExpr asgFun $ \case
            ASGFunction{} -> return True
            ClsASG{}      -> return False
    nodeCache <- use $ Graph.userState . Graph.clsNodeCache
    uuid      <- maybe (liftIO UUID.nextRandom) return lastUUID
    if isFun then do
        (funName, ref, fileOffset) <- runASTOp $ do
            putLayer @Marker fun . Just =<< toPortMarker (OutPortRef (convert uuid) [])
            asgFun    <- ASTRead.cutThroughDocAndMarked fun
            matchExpr asgFun $ \case
                ASGFunction n _ _ -> do
                    offset <- functionBlockStartRef asgFun
                    name   <- handle (\(_e::ASTRead.InvalidNameException) -> return "")
                        $ ASTRead.getVarName' =<< source n
                    return (nameToString name, asgFun, offset)
        let oldPortMapping = nodeCache ^. portMappingMap . at (uuid, Nothing)
        portMapping <- fromJustM (liftIO $ (,) <$> UUID.nextRandom <*> UUID.nextRandom) oldPortMapping
        globalMarkers <- use $ Graph.userState . Graph.clsCodeMarkers
        let bh       = BH.LamItem portMapping ref def
            funGraph = Graph.Graph bh def globalMarkers def def fileOffset nodeCache
            graph    = Graph.FunctionDefinition (Graph.FunctionGraph funName funGraph Map.empty)
        Graph.userState . Graph.clsFuns . at uuid ?= graph
        updatedCache <- withRootedFunction uuid (Graph._FunctionDefinition) $ do
            runASTOp $ do
                markers <- extractMarkers ref
                let localMarkers = Map.filterWithKey (\k _ -> k `elem` markers) globalMarkers
                Graph.codeMarkers .= localMarkers
                propagateLengths ref
            runAliasAnalysis
            runASTOp $ do
                ASTBreadcrumb.makeTopBreadcrumbHierarchy ref
                ASTBreadcrumb.restorePortMappings uuid (nodeCache ^. portMappingMap)
                use Graph.graphNodeCache
        Graph.userState . Graph.clsNodeCache .= updatedCache
        graph' <- case className of
            Nothing -> pure graph
            Just (n, clsId) -> do
                Just newGraph <- use $ Graph.userState . Graph.clsFuns . at uuid
                Graph.userState . Graph.clsFuns . at uuid .= Nothing
                return newGraph
        return (uuid, graph')
    else do
        (className, ref, fileOffset, meths) <- runASTOp $ do
            putLayer @Marker fun . Just =<< toPortMarker (OutPortRef (convert uuid) [])
            asgFun    <- ASTRead.cutThroughDocAndMarked fun
            matchExpr asgFun $ \case
                ClsASG _ n _ _ decls -> do
                    offset <- functionBlockStartRef asgFun
                    meths <- ptrListToList decls >>= mapM source
                    return (nameToString n, asgFun, offset, meths)
        mets <- forM meths $ \a -> makeGraphCls a (Just (className, uuid)) Nothing
        liftIO $ print className >>
            print (mets & traverse . _2 %~ (^?! (Graph._FunctionDefinition . Graph.funName))) >>
            IO.hFlush IO.stdout
        let graph = Graph.ClassDefinition (Graph.ClassGraph className (Map.fromList $ map (over _2 (^?! Graph._FunctionDefinition)) mets))
        Graph.userState . Graph.clsFuns . at uuid ?= graph
        return (uuid, graph)


runInternalBreadcrumb :: Breadcrumb BreadcrumbItem -> Command Graph.Graph a
    -> Command Graph.Graph a
runInternalBreadcrumb breadcrumb act = do
    graph <- get
    let breadcrumbHierarchy
            = graph ^. Graph.userState . Graph.breadcrumbHierarchy
    case breadcrumbHierarchy `navigateTo` breadcrumb of
        Just h -> do
            env <- ask
            let newGraph
                    = graph & Graph.userState . Graph.breadcrumbHierarchy .~ h
            (res, state) <- liftIO $ runEmpire env newGraph act
            let modified = replaceAt breadcrumb breadcrumbHierarchy
                    $ state ^. Graph.userState . Graph.breadcrumbHierarchy
            newBc <- maybe
                (throwM $ BH.BreadcrumbDoesNotExistException breadcrumb)
                return
                modified
            let newGraph' = state & Graph.userState . Graph.breadcrumbHierarchy
                    .~ newBc
            put newGraph'
            return res
        _ -> throwM $ BH.BreadcrumbDoesNotExistException breadcrumb


-- withRootedFunction :: NodeId -> Command Graph.Graph a
--     -> Command Graph.ClsGraph a
-- withRootedFunction uuid act = do
--     graph <- preuse
--         (Graph.userState . Graph.clsFuns . ix uuid . Graph.funGraph)
--         <?!> BH.BreadcrumbDoesNotExistException (Breadcrumb [Definition uuid])
--     env      <- ask
--     state    <- get
--     clsGraph <- use Graph.userState
--     functionMarkers
--         <- use $ Graph.userState . Graph.clsFuns . ix uuid . Graph.funMarkers
--     let properGraph = let clsCode        = clsGraph ^. Graph.code
--                           clsCodeMarkers = clsGraph ^. Graph.clsCodeMarkers
--                           clsParseError  = clsGraph ^. Graph.clsParseError
--                       in graph & Graph.code .~ clsCode
--                                & Graph.codeMarkers .~ functionMarkers
--                                & Graph.globalMarkers .~ clsCodeMarkers
--                                & Graph.parseError .~ clsParseError
--         properState = state & Graph.userState .~ properGraph

--     ((res, len), newGraph) <- liftIO $ runEmpire env properState $ do
--         a   <- act
--         len <- runASTOp $ getLayer @SpanLength =<< ASTRead.getCurrentASTRef
--         return (a, len)
--     Graph.userState . Graph.clsFuns . ix uuid . Graph.funGraph
--         .= newGraph ^. Graph.userState

--     let lookupSpan f = view CodeSpan.realSpan <$> getLayer @CodeSpan.CodeSpan f
--         updateLayer fun off spanLen = putLayer @CodeSpan.CodeSpan fun
--             $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan off spanLen))

--     diffs <- runASTOp $ do
--         cls <- use Graph.clsClass
--         funs <- ASTRead.classFunctions cls
--         forM funs $ \fun -> ASTRead.cutThroughDocAndMarked fun >>= \f ->
--             matchExpr f $ \case
--                 ASGFunction _ _ _ -> do
--                     nodeId <- ASTRead.getNodeId fun
--                     if nodeId == Just uuid then Just <$>
--                         if fun == f then do
--                             LeftSpacedSpan (SpacedSpan off prevLen)
--                                 <- lookupSpan f
--                             updateLayer fun off len
--                             pure $ len - prevLen
--                         else do
--                             LeftSpacedSpan (SpacedSpan off funLen)
--                                 <- lookupSpan f
--                             updateLayer f off len
--                             let diff = len - funLen
--                             LeftSpacedSpan (SpacedSpan off' markedLen)
--                                 <- lookupSpan fun
--                             updateLayer fun off' $ markedLen + diff
--                             pure diff
--                     else pure Nothing

--     let diff = fromMaybe
--             (error "function not in AST?")
--             . listToMaybe $ catMaybes diffs
--         funOffset = properGraph ^. Graph.fileOffset
--     Graph.userState . Graph.clsFuns . traverse . Graph.funGraph
--         . Graph.fileOffset %= (\a -> if a > funOffset then a + diff else a)
--     Graph.userState . Graph.clsFuns . ix uuid . Graph.funMarkers
--         .= newGraph ^. Graph.userState . Graph.codeMarkers
--     Graph.userState . Graph.clsCodeMarkers
--         %= \m -> Map.union m (newGraph ^. Graph.userState . Graph.codeMarkers)
--     Graph.userState . Graph.code
--         .= newGraph ^. Graph.userState . Graph.code
--     Graph.userState . Graph.clsParseError
--         .= newGraph ^. Graph.userState . Graph.parseError
--     pure res

zoomInternalBreadcrumb :: Breadcrumb BreadcrumbItem -> Command Graph.Graph a
    -> Command Graph.Graph a
zoomInternalBreadcrumb (Breadcrumb (Definition _ : rest))
    = zoomInternalBreadcrumb (Breadcrumb rest)
zoomInternalBreadcrumb breadcrumb = runInternalBreadcrumb breadcrumb

zoomBreadcrumb
    :: Breadcrumb BreadcrumbItem
    -> Command Graph.Graph a
    -> Command Graph.ClsGraph a
    -> Command Library.Library a
zoomBreadcrumb = zoomCommand Library.body .:. zoomBreadcrumb' -- where
    -- zoomBreadcrumb' (Breadcrumb [])                       _actG  actC = actC
    -- zoomBreadcrumb' (Breadcrumb (Definition uuid : rest))  actG _actC
    --     = withRootedFunction uuid $ runInternalBreadcrumb (Breadcrumb rest) actG
    -- zoomBreadcrumb' breadcrumb _ _
    --     = throwM $ BH.BreadcrumbDoesNotExistException breadcrumb


withRootedFunction :: NodeId -> Traversal' Graph.TopLevelGraph Graph.FunctionGraph
    -> Command Graph.Graph a -> Command Graph.ClsGraph a
withRootedFunction uuid funLens act = do
    liftIO $ print $ "rooting on " <> show uuid
    graph    <- preuse (Graph.userState . Graph.clsFuns . ix uuid . funLens . Graph.funGraph) <?!> BH.BreadcrumbDoesNotExistException (Breadcrumb [Definition uuid])
    env      <- ask
    state    <- get
    clsGraph <- use Graph.userState
    functionMarkers <- use $ Graph.userState . Graph.clsFuns . ix uuid . funLens . Graph.funMarkers
    let properGraph = let clsCode        = clsGraph ^. Graph.code
                          clsCodeMarkers = clsGraph ^. Graph.clsCodeMarkers
                          clsParseError  = clsGraph ^. Graph.clsParseError
                      in graph & Graph.code .~ clsCode
                               & Graph.codeMarkers .~ functionMarkers
                               & Graph.globalMarkers .~ clsCodeMarkers
                               & Graph.parseError .~ clsParseError
    ((res, len), newGraph) <- liftIO $ runEmpire env (state & Graph.userState .~ properGraph) $ do
        a <- act
        len <- runASTOp $ do
            ref <- ASTRead.getCurrentASTRef
            getLayer @SpanLength ref
        return (a, len)
    Graph.userState . Graph.clsFuns . ix uuid . funLens . Graph.funGraph .= newGraph ^. Graph.userState
    diffs <- runASTOp $ do
        cls <- use Graph.clsClass
        funs <- ASTRead.unitDefinitions cls
        forM funs $ \fun -> ASTRead.cutThroughDocAndMarked fun >>= \f -> matchExpr f $ \case
            ASGFunction n _ _ -> do
                nodeId <- ASTRead.getNodeId fun
                n' <- ASTRead.getNameOf =<< source n
                -- liftIO $ print ("fun: " <> show n' <> " " <> show nodeId) >> IO.hFlush IO.stdout
                if (nodeId == Just uuid) then do
                    lenDiff <- if fun == f then do
                        LeftSpacedSpan (SpacedSpan off prevLen) <- view CodeSpan.realSpan <$> getLayer @CodeSpan.CodeSpan f
                        putLayer @CodeSpan.CodeSpan fun $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan off len))
                        return $ len - prevLen
                    else do
                        LeftSpacedSpan (SpacedSpan off funLen) <- view CodeSpan.realSpan <$> getLayer @CodeSpan.CodeSpan f
                        putLayer @CodeSpan.CodeSpan f $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan off len))
                        let diff = len - funLen
                        LeftSpacedSpan (SpacedSpan off markedLen) <- view CodeSpan.realSpan <$> getLayer @CodeSpan.CodeSpan fun
                        putLayer @CodeSpan.CodeSpan fun $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan off (markedLen + diff)))
                        return diff
                    -- Just (funExpr :: Expr IR.ASGRootedFunction) <- narrowTerm f
                    -- let newRooted = IR.Rooted (newGraph ^. Graph.ast . Graph.ir) (newGraph ^. Graph.breadcrumbHierarchy . BH.self)
                    -- IR.modifyExprTerm funExpr $ wrapped . IR.termASGRootedFunction_body .~ newRooted
                    return $ Just lenDiff
                    else return Nothing
            ClsASG _ n _ _ _ -> do
                nodeId <- ASTRead.getNodeId fun
                -- liftIO $ print ("cls: " <> nameToString n <> " " <> show nodeId) >> IO.hFlush IO.stdout
                if (nodeId == Just uuid) then do
                    lenDiff <- if fun == f then do
                        LeftSpacedSpan (SpacedSpan off prevLen) <- view CodeSpan.realSpan <$> getLayer @CodeSpan.CodeSpan f
                        putLayer @CodeSpan.CodeSpan fun $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan off len))
                        return $ len - prevLen
                    else do
                        LeftSpacedSpan (SpacedSpan off funLen) <- view CodeSpan.realSpan <$> getLayer @CodeSpan.CodeSpan f
                        putLayer @CodeSpan.CodeSpan f $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan off len))
                        let diff = len - funLen
                        LeftSpacedSpan (SpacedSpan off markedLen) <- view CodeSpan.realSpan <$> getLayer @CodeSpan.CodeSpan fun
                        putLayer @CodeSpan.CodeSpan fun $ CodeSpan.mkRealSpan (LeftSpacedSpan (SpacedSpan off (markedLen + diff)))
                        return diff
                    -- Just (funExpr :: Expr IR.ASGRootedFunction) <- narrowTerm f
                    -- let newRooted = IR.Rooted (newGraph ^. Graph.ast . Graph.ir) (newGraph ^. Graph.breadcrumbHierarchy . BH.self)
                    -- IR.modifyExprTerm funExpr $ wrapped . IR.termASGRootedFunction_body .~ newRooted
                    return $ Just lenDiff
                    else return Nothing
    let diff = fromMaybe 0 $ listToMaybe $ catMaybes diffs
        funOffset = properGraph ^. Graph.fileOffset
    Graph.userState . Graph.clsFuns . traverse . funLens . Graph.funGraph . Graph.fileOffset %= (\a -> if a > funOffset then a + diff else a)
    Graph.userState . Graph.clsFuns . ix uuid . funLens . Graph.funMarkers .= newGraph ^. Graph.userState . Graph.codeMarkers
    Graph.userState . Graph.clsCodeMarkers %= \m -> Map.union m (newGraph ^. Graph.userState . Graph.codeMarkers)
    Graph.userState . Graph.code           .= newGraph ^. Graph.userState . Graph.code
    Graph.userState . Graph.clsParseError  .= newGraph ^. Graph.userState . Graph.parseError
    return res

-- withRootedClass ::
--     NodeId ->
--     Breadcrumb BreadcrumbItem ->
--     (Breadcrumb BreadcrumbItem -> Command Graph.Graph a) ->
--     Command Graph.ClsGraph a
-- withRootedClass uuid bc@(Breadcrumb []) act = act bc
-- withRootedClass uuid (Breadcrumb (Definition funuuid:rest)) act = act (Breadcrumb rest)



zoomBreadcrumb' :: Breadcrumb BreadcrumbItem -> Command Graph.Graph a -> Command Graph.ClsGraph a -> Command Graph.ClsGraph a
zoomBreadcrumb' (Breadcrumb []) _actG actC = actC
zoomBreadcrumb' breadcrumb@(Breadcrumb (Definition uuid : rest)) actG _actC = do
    liftIO $ print $ "zooming on " <> show breadcrumb
    topLvlGraph <- preuse (Graph.userState . Graph.clsFuns . ix uuid)
        <?!> BH.BreadcrumbDoesNotExistException breadcrumb
    case topLvlGraph of
        Graph.FunctionDefinition _ -> withRootedFunction uuid (Graph._FunctionDefinition :: Traversal' Graph.TopLevelGraph Graph.FunctionGraph) $
            runInternalBreadcrumb (Breadcrumb rest) actG
        Graph.ClassDefinition _    ->
            case rest of
                Definition funUUID : rest' ->
                    withRootedFunction uuid (Graph._ClassDefinition . Graph.classMethods . ix funUUID) $ runInternalBreadcrumb (Breadcrumb rest') actG
                rest' -> (print =<< use Graph.code) >> error "foo"
zoomBreadcrumb' breadcrumb _ _ = throwM $ BH.BreadcrumbDoesNotExistException breadcrumb
