{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Empire.Commands.Typecheck where

import Control.Lens (uses, element)
import           Control.Arrow                    ((***), (&&&))
import           Control.Concurrent.Async         (Async)
import qualified Control.Concurrent.Async         as Async
import           Control.Concurrent               (MVar, readMVar)
import qualified Control.Concurrent.MVar.Lifted   as Lifted
import           Control.Exception.Safe           (mask_)
import           Control.Monad                    (void)
import           Control.Monad.Except             hiding (when)
import           Control.Monad.Reader             (ask, runReaderT)
import           Control.Monad.State              (execStateT)
import           Data.Graph.Data.Component.Class  (Component(..))
import qualified Data.Graph.Store                 as Store
import qualified Data.Graph.Store.Buffer          as Buffer
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, maybeToList)
import           Empire.Prelude                   hiding (mapping, toList)
import           Foreign.Ptr                      (plusPtr)
import           System.Directory                 (canonicalizePath, withCurrentDirectory)
import           System.Environment               (getEnv)
import           System.FilePath                  (takeDirectory)
import qualified Path

import qualified Luna.IR                          as IR
import qualified Luna.Pass.Typing.Typechecker     as TC
import qualified Luna.Pass.Preprocess.PreprocessDef as Prep
import qualified Memory                           as Memory
import           LunaStudio.Data.Breadcrumb       (Breadcrumb (..), BreadcrumbItem(Definition))
import           LunaStudio.Data.GraphLocation    (GraphLocation (..))
import qualified LunaStudio.Data.Error            as APIError
import           LunaStudio.Data.NodeValue        (NodeValue (..))
import           LunaStudio.Data.Visualization    (VisualizationValue (..))

import           Empire.ASTOp                     (liftScheduler, runASTOp)
-- import           Empire.ASTOp                     (getImportedModules, runASTOp, runTypecheck, runModuleTypecheck)
import qualified Empire.ASTOps.Read               as ASTRead
import           Empire.Commands.Breadcrumb       (runInternalBreadcrumb, withRootedFunction)
import qualified Empire.Commands.GraphBuilder     as GraphBuilder
import qualified Empire.Commands.Publisher        as Publisher
import           Empire.Data.BreadcrumbHierarchy  (topLevelIDs)
import qualified Empire.Data.BreadcrumbHierarchy  as BH
import           Empire.Data.AST                  (NodeRef)
import           Empire.Data.Graph                (Graph)
import qualified Empire.Data.Graph                as Graph
import           Empire.Empire

import qualified System.IO as IO
import qualified Luna.Debug.IR.Visualizer as Vis

import qualified Luna.Pass.Resolve.Data.Resolution as Resolution
import qualified Luna.Pass.Sourcing.UnitMapper     as UnitMapper
import           Luna.Pass.Data.Stage (Stage)
import qualified Data.Bimap as Bimap
import qualified Luna.Pass.Flow.ProcessUnits       as ProcessUnits
import qualified Data.Graph.Data.Layer.Layout      as Layout

import qualified Luna.Pass.Scheduler                 as Scheduler
import qualified Luna.Pass.Sourcing.UnitLoader as UnitLoader
import qualified Luna.Pass.Sourcing.ImportsPlucker as ImportsPlucker
import qualified Luna.Std as Std
import qualified Luna.Pass.Sourcing.Data.Unit  as Unit

import qualified Data.Set as Set

import Data.Map (Map)
-- import           Luna.Builtin.Data.LunaEff        (runError, runIO)
-- import           Luna.Builtin.Data.Module         (Imports (..), unionImports, unionsImports)
-- import           Luna.Builtin.Prim                (SingleRep (..), ValueRep (..), getReps)
-- import qualified Luna.Compilation                 as Compilation
import qualified Luna.Project                     as Project
-- import           Luna.Compilation                 (CompiledModules (..))
import qualified Luna.IR                          as IR
-- import           Luna.Pass.Data.ExprMapping
-- import qualified Luna.Pass.Evaluation.Interpreter as Interpreter
-- import           Luna.Pass.Resolution.Data.CurrentTarget (CurrentTarget(TgtDef))
-- import qualified Luna.IR.Layer.Errors             as Errors
-- import           OCI.IR.Name.Qualified            (QualName)

-- runTC :: QualName -> String -> Imports -> Command Graph ()
runTC moduleName functionName imports = return () -- do
    -- let currentTarget = TgtDef (convert moduleName) (convert functionName)
    -- runTypecheck currentTarget imports
    -- runASTOp $ do
    --     mapping <- unwrap <$> IR.getAttr @ExprMapping
    --     Graph.breadcrumbHierarchy . BH.refs %= (\x -> Map.findWithDefault x x mapping)
    -- return ()

-- runInterpreter :: FilePath -> Imports -> Command Graph (Maybe Interpreter.LocalScope)
runInterpreter path imports = return Nothing -- runASTOp $ do
    -- rootPath <- liftIO $ Project.findProjectRootForFile =<< Path.parseAbsFile path
    -- selfRef  <- use $ Graph.breadcrumbHierarchy . BH.self
    -- IR.matchExpr selfRef $ \case
    --     IR.ASGFunction _ [] b -> do
    --         bodyRef <- IR.source b
    --         res     <- Interpreter.interpret' imports . IR.unsafeGeneralize $ bodyRef
    --         mask_ $ do
    --             result  <- liftIO $ withCurrentDirectory (maybe (takeDirectory path) Path.toFilePath rootPath) $ runIO $ runError $ execStateT res def
    --             case result of
    --                 Left e  -> return Nothing
    --                 Right r -> return $ Just r
    --     _ -> return Nothing

updateNodes :: GraphLocation -> Command Graph ()
updateNodes loc@(GraphLocation _ br) = do
    -- (inEdge, outEdge) <- use $ Graph.breadcrumbHierarchy . BH.portMapping
    (updates) <- runASTOp $ do
        -- sidebarUpdates <- (\x y -> [x, y]) <$> GraphBuilder.buildInputSidebarTypecheckUpdate  inEdge
        --                                    <*> GraphBuilder.buildOutputSidebarTypecheckUpdate outEdge
        allNodeIds  <- uses Graph.breadcrumbHierarchy topLevelIDs
        nodeUpdates <- mapM GraphBuilder.buildNodeTypecheckUpdate allNodeIds
        -- errors      <- forM allNodeIds $ \nid -> do
        --     errs <- IR.getLayer @IR.Errors =<< ASTRead.getASTRef nid
        --     case errs of
        --         []     -> return Nothing
        --         e : es -> do
        --             let toSrcLoc (Errors.ModuleTagged mod (Errors.FromMethod klass method)) = APIError.SourceLocation (convert mod) (Just (convert klass)) (convert method)
        --                 toSrcLoc (Errors.ModuleTagged mod (Errors.FromFunction function))   = APIError.SourceLocation (convert mod) Nothing (convert function)
        --                 errorDetails = APIError.CompileErrorDetails (map toSrcLoc (e ^. Errors.arisingFrom)) (map toSrcLoc (e ^. Errors.requiredBy))
        --             return $ Just $ (nid, NodeError $ APIError.Error (APIError.CompileError errorDetails) $ e ^. Errors.description)
        return (nodeUpdates)
    mask_ $ do
        traverse_ (Publisher.notifyNodeTypecheck loc) updates
        -- for_ (catMaybes errors) $ \(nid, e) -> Publisher.notifyResultUpdate loc nid e 0

updateValues :: GraphLocation
             -> a
             -> Command Graph [Async ()]
updateValues loc scope = return [] -- do
    -- childrenMap <- use $ Graph.breadcrumbHierarchy . BH.children
    -- let allNodes = Map.assocs $ view BH.self <$> childrenMap
    -- env     <- ask
    -- allVars <- runASTOp $ fmap catMaybes $ forM allNodes $ \(nid, tgt) -> do
    --     pointer <- ASTRead.getASTPointer nid
    --     IR.matchExpr pointer $ \case
    --         IR.Unify{} -> Just . (nid,) <$> ASTRead.getVarNode pointer
    --         _          -> return Nothing
    -- let send nid m = flip runReaderT env $
    --         Publisher.notifyResultUpdate loc nid m 0
    --     sendRep nid (ErrorRep e)     = send nid
    --                                  $ NodeError
    --                                  $ APIError.Error APIError.RuntimeError
    --                                  $ convert e
    --     sendRep nid (SuccessRep s l) = send nid
    --                                  $ NodeValue (convert s)
    --                                  $ Value . convert <$> l
    --     sendStreamRep nid a@(ErrorRep _)   = sendRep nid a
    --     sendStreamRep nid (SuccessRep s l) = send nid
    --                                        $ NodeValue (convert s)
    --                                        $ StreamDataPoint . convert <$> l
    -- asyncs <- forM allVars $ \(nid, ref) -> do
    --     let resVal = Interpreter.localLookup (IR.unsafeGeneralize ref) scope
    --     liftIO $ forM resVal $ \v -> do
    --         value <- getReps v
    --         case value of
    --             OneTime r   -> Async.async $ sendRep nid r
    --             Streaming f -> do
    --                 send nid (NodeValue "Stream" $ Just StreamStart)
    --                 Async.async (f (sendStreamRep nid))
    -- return $ catMaybes asyncs

flushCache :: Command InterpreterEnv ()
flushCache = do
    -- errorsCache .= def
    -- valuesCache .= def
    -- nodesCache  .= def
    return ()

-- newtype Scope = Scope CompiledModules
-- makeWrapped ''Scope

-- flattenScope :: Scope -> Imports
-- flattenScope (Scope (CompiledModules mods prims)) =
--     unionsImports $ prims : Map.elems mods

-- createStdlib :: String -> IO (IO (), Scope)
-- createStdlib =
--     fmap (id *** Scope) . Compilation.prepareStdlib . Map.singleton "Std"

filePathToQualName :: MonadIO m => FilePath -> m IR.Qualified
filePathToQualName path = liftIO $ do
    path' <- Path.parseAbsFile path
    root  <- Project.projectRootForFile path'
    let projName = Project.getProjectName root
    file  <- Path.stripProperPrefix (root Path.</> $(Path.mkRelDir "src")) path'
    return $ Project.mkQualName projName file

fileImportPaths :: MonadIO m => FilePath -> m (Map IR.Qualified FilePath)
fileImportPaths file = liftIO $ do
    filePath        <- Path.parseAbsFile file
    currentProjPath <- Project.projectRootForFile filePath
    libs            <- Project.projectImportPaths currentProjPath
    srcs            <- for (snd <$> libs) $ \libPath -> do
        p <- Path.parseAbsDir libPath
        fmap Path.toFilePath . Bimap.toMapR <$> Project.findProjectSources p
    pure $ Map.unions srcs

-- recomputeCurrentScope :: MVar CompiledModules
--                       -> FilePath
--                       -> Command InterpreterEnv Imports
-- recomputeCurrentScope imports file = do
--     Lifted.modifyMVar imports $ \imps -> do
--         importPaths     <- liftIO $ do
--             filePath        <- Path.parseAbsFile file
--             currentProjPath <- Project.projectRootForFile filePath
--             Project.projectImportPaths currentProjPath
--         qualName        <- filePathToQualName file
--         (f, nimps)      <- zoom graph $ do
--             t <- runModuleTypecheck qualName (Map.fromList importPaths) imps
--             case t of
--                 Right (a, b) -> return (a, b)
--                 Left e  -> error $ show e <> " " <> file
--         let nimpsF = nimps & Compilation.modules . at qualName ?~ f
--         return (nimpsF, f)

-- getCurrentScope :: MVar CompiledModules
--                 -> FilePath
--                 -> Command InterpreterEnv Imports
-- getCurrentScope imports file = do
--     fs       <- liftIO $ readMVar imports
--     qualName <- filePathToQualName file
--     case fs ^. Compilation.modules . at qualName of
--         Just f -> return f
--         _      -> recomputeCurrentScope imports file

stop :: Command InterpreterEnv ()
stop = do
    return ()
    -- cln       <- use cleanUp
    -- threads   <- use listeners
    -- listeners .= []
    -- liftIO $ mapM_ Async.uninterruptibleCancel threads
    -- liftIO cln

translate :: Buffer.RedirectMap -> Int -> NodeRef -> NodeRef 
translate redMap off node = wrap $ (unwrap (redMap Map.! (Memory.Ptr $ convert node)))
    `plusPtr` off

primStdModuleName :: IR.Qualified
primStdModuleName = "Std.Primitive"

makePrimStdIfMissing :: FilePath -> Command InterpreterEnv ()
makePrimStdIfMissing file = do
    existingStd <- use $ Graph.userState . resolvers . at primStdModuleName
    case existingStd of
        Just _ -> return ()
        Nothing -> do
            print $ "STD IS MISSING"
            liftIO $ IO.hFlush IO.stdout
            (finalizer, typed, computed, ress) <- liftScheduler $ do
                (fin, stdUnitRef) <- Std.stdlib @Stage
                srcs <- fileImportPaths file
                UnitLoader.init
                Scheduler.registerAttr @Unit.UnitRefsMap
                Scheduler.setAttr $ Unit.UnitRefsMap $ Map.singleton "Std.Primitive" stdUnitRef
                for Std.stdlibImports $ UnitLoader.loadUnit def srcs []
                Unit.UnitRefsMap mods <- Scheduler.getAttr
                units <- flip Map.traverseWithKey mods $ \n u -> case u ^. Unit.root of
                    Unit.Graph r -> UnitMapper.mapUnit n r
                    Unit.Precompiled u -> pure u
                let unitResolvers   = Map.mapWithKey Resolution.resolverFromUnit units
                    importResolvers = Map.mapWithKey (Resolution.resolverForUnit unitResolvers) $ over wrapped ("Std.Base" :)
                                                                                         . over wrapped ("Std.Primitive" :)
                                                                                         . view Unit.imports <$> mods
                    unitsWithResolvers = Map.mapWithKey (\n u -> (importResolvers Map.! n, u)) units
                (typed, evald) <- ProcessUnits.processUnits def def unitsWithResolvers
                return (fin, typed, evald, unitResolvers)
            Graph.userState . cleanUp      .= finalizer
            Graph.userState . typedUnits   .= typed
            Graph.userState . runtimeUnits .= computed
            Graph.userState . resolvers    .= ress


compileCurrentScope :: Bool -> FilePath -> NodeRef -> Command InterpreterEnv ()
compileCurrentScope recompute path root = do
    typed   <- use $ Graph.userState . typedUnits
    evald   <- use $ Graph.userState . runtimeUnits
    ress    <- use $ Graph.userState . resolvers
    modName <- filePathToQualName path

    (newTyped, newEvald, newResolvers) <- liftScheduler $ do
        imports <- ImportsPlucker.run root
        UnitLoader.init
        srcs <- fileImportPaths path
        Scheduler.registerAttr @Unit.UnitRefsMap
        Scheduler.setAttr $ Unit.UnitRefsMap $ Map.singleton modName $ Unit.UnitRef (Unit.Graph $ Layout.unsafeRelayout root) (wrap imports)
        print $ "known: " <> show (Map.keys ress)
        liftIO $ IO.hFlush IO.stdout
        for imports $ UnitLoader.loadUnitIfMissing (Set.fromList $ Map.keys ress) srcs [modName]

        Unit.UnitRefsMap mods <- Scheduler.getAttr
        print $ Map.keys mods
        liftIO $ IO.hFlush IO.stdout
        units <- flip Map.traverseWithKey mods $ \n u -> case u ^. Unit.root of
            Unit.Graph r -> UnitMapper.mapUnit n r
            Unit.Precompiled u -> pure u

        let unitResolvers   = Map.union (Map.mapWithKey Resolution.resolverFromUnit units) ress
            importResolvers = Map.mapWithKey (Resolution.resolverForUnit unitResolvers) $ over wrapped ("Std.Base" :)
                                                                                 . over wrapped ("Std.Primitive" :)
                                                                                 . view Unit.imports <$> mods
            unitsWithResolvers = Map.mapWithKey (\n u -> (importResolvers Map.! n, u)) units
        (newTyped, newEvald) <- ProcessUnits.processUnits typed evald unitsWithResolvers
        return (newTyped, newEvald, unitResolvers)

    Graph.userState . typedUnits   .= newTyped
    Graph.userState . runtimeUnits .= newEvald
    Graph.userState . resolvers    .= newResolvers

run :: GraphLocation
    -> Graph.ClsGraph
    -> Store.RootedWithRedirects NodeRef
    -> Bool
    -> Bool
    -> Command InterpreterEnv ()
run loc@(GraphLocation file br) clsGraph' rooted' interpret recompute = do
    let Store.RootedWithRedirects rooted redMap = rooted'
    (a, redirects) <- runASTOp $ Store.deserializeWithRedirects rooted
    let originalCls = clsGraph' ^. Graph.clsClass
        deserializerOff = redirects Map.! someTypeRep @IR.Terms
        newCls = translate redMap deserializerOff originalCls
    print newCls
    liftIO $ IO.hFlush IO.stdout
    let newClsGraph = clsGraph' & BH.refs %~ translate redMap deserializerOff
    Graph.userState . clsGraph .= newClsGraph

    makePrimStdIfMissing file
    compileCurrentScope recompute file a

    let [fun] = newClsGraph ^. Graph.clsFuns . to Map.elems
    typed <- use $ Graph.userState . typedUnits
    resolver <- fmap (mconcat . Map.elems) $ use $ Graph.userState . resolvers
    zoomCommand clsGraph $ do
        cls <- use $ Graph.userState
        case br of
            Breadcrumb [] -> do
                return ()
            Breadcrumb (Definition uuid:rest) -> do
                let Just root = cls ^? Graph.clsFuns . ix uuid . Graph.funGraph . Graph.breadcrumbHierarchy . BH.self
                liftScheduler $ do
                    Prep.preprocessDef resolver root
                    TC.runTypechecker def root typed
                withRootedFunction uuid $ do
                    runInternalBreadcrumb (Breadcrumb rest) $ do
                        updateNodes  loc
        -- updateNodes loc
    return ()
            -- void $ mask_ $ recomputeCurrentScope imports file
    --         let scopeGetter = if recompute
    --                           then recomputeCurrentScope
    --                           else getCurrentScope
    --         scope  <- mask_ $ scopeGetter imports file
    --         asyncs <- zoom graph $ do
    --             importedModules <- getImportedModules
    --             std             <- liftIO $ readMVar imports
    --             let CompiledModules cmpMods cmpPrims = std
    --                 cmpImportedMods = Map.restrictKeys cmpMods importedModules
    --                 visibleModules = CompiledModules cmpImportedMods cmpPrims
    --                 flatVisibleScope = flattenScope $ Scope visibleModules
    --                 moduleEnv      = unionImports flatVisibleScope scope
    --             modName <- filePathToQualName file
    --             funName <- preuse $
    --                 Graph.clsFuns . at uuid . _Just . Graph.funName
    --                     let functionName = fromMaybe "unknown function" funName
    --                     runTC modName functionName moduleEnv
    --                     {-updateMonads loc-}
    --                     if interpret then do
    --                         scope  <- runInterpreter file moduleEnv
    --                         traverse (updateValues loc) scope
    --                     else return Nothing
    --         listeners .= fromMaybe [] asyncs
