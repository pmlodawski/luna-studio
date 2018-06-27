{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PartialTypeSignatures      #-}

module Empire.ASTOp (
    ASTOp
  , ClassOp
  , GraphOp
  , ASTOpReq
  , EmpirePass
  , Printer
  , defaultClsGraph
  , defaultPMState
  -- , PMStack
  -- , getImportedModules
  -- , putNewIR
  -- , putNewIRCls
  , runAliasAnalysis
  , runASTOp
  -- , runPass
  -- , runPM
  -- , runTypecheck
  -- , runModuleTypecheck
  , match
  ) where

import           Empire.Prelude       hiding (Type, mempty, toList)
import           Prologue             (mempty)

import           Control.Monad.Catch  (MonadCatch(..))
-- import           Control.Monad.Raise  (MonadException)
import           Control.Monad.State  (MonadState, StateT(..), runStateT, get, put)
-- import qualified Control.Monad.State.Dependent as DepState
import qualified Control.Monad.State.Layered as Layered
import qualified Data.TypeMap.MultiState as MultiState
import           Data.IORef
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Empire.Data.Graph    (AST(..), ClsGraph, Graph, withVis)
import qualified Empire.Data.Graph    as Graph
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Layers   (Marker, Meta, TypeLayer, SpanLength, SpanOffset, attachEmpireLayers)
import           Empire.Empire        (Command, CommandStack)
import           GHC.Exts             (Any)
import           Unsafe.Coerce        (unsafeCoerce)

import qualified Luna.Pass        as Pass
import qualified OCI.Pass.Definition.Declaration as Pass

import           Luna.IR              as IR hiding (Unit, String, Marker, match, source)
-- import qualified Luna.IR.Term.Unit    as Term
-- import           OCI.IR.Layout.Typed  (type (>>))
import Foreign.Info.ByteSize (ByteSize)
import Foreign.Memory.Pool (MemPool)
import           Data.Graph.Data.Component.Class   (Component)
import qualified Data.Graph.Data.Component.Class   as Component
import qualified Data.Graph.Component.Node.Layer   as Layer
import qualified Data.Graph.Data.Layer.Class   as Layer
import           Data.Graph.Component.Edge.Class   (Edge, Edges)
import qualified Data.Graph.Data.Graph.Class   as LunaGraph
import qualified Data.Graph.Component.Node.Destruction as Destruct
import           Data.Graph.Component.Node.Class (Node, Nodes)
-- import           OCI.IR.Name.Qualified (QualName)
import           OCI.Pass.Definition.Class       (Pass(..)) --, ComponentSize, LayerMemManager, ComponentMemPool)
-- import           OCI.Pass.Class       (Inputs, Outputs, Preserves, KnownPass)
-- import           OCI.IR.Class         (Import)
-- import qualified OCI.Pass.Class       as Pass (SubPass, eval')
-- import qualified OCI.Pass.Manager     as Pass (PassManager, setAttr, State)
-- import qualified Luna.Runner as Runner
import qualified OCI.Pass.Management.Scheduler as Scheduler
import qualified Data.Graph.Data.Graph.Class as LunaGraph
import qualified Luna.Pass.Attr as Attr

-- import           System.Log                                   (DropLogger(..), dropLogs)
-- import           System.Log.Logger.Class                      (Logger(..), IdentityLogger(..))
import qualified Empire.Pass.PatternTransformation            as PatternTransformation
-- import           Luna.Pass.Resolution.Data.UnresolvedConses   (UnresolvedConses(..), NegativeConses(..))
import           Luna.Pass.Resolve.Data.UnresolvedVariables     (UnresolvedVariables(..))
import           Luna.Pass.Data.Root                     (Root(..))
import qualified Luna.Pass.Resolve.AliasAnalysis           as AliasAnalysis
import           Luna.Syntax.Text.Parser.Data.Invalid (Invalids)
-- import qualified Parser.Parser               as Parser
import           Luna.Syntax.Text.Parser.Data.CodeSpan        (CodeSpan)
-- import           Luna.Syntax.Text.Parser.Marker               (MarkedExprMap)
-- import           Luna.Syntax.Text.Source                      (Source)
-- import qualified Luna.Pass.Typechecking.Typecheck             as Typecheck
-- import qualified Luna.Compilation                             as Compilation
-- import           Luna.Compilation                             (CompiledModules (..))

-- import qualified OCI.IR.Repr.Vis                   as Vis
-- import qualified Control.Monad.State.Dependent.Old as DepOld
-- import           Luna.Pass.Data.UniqueNameGen               (initNameGen)
-- import           Luna.Pass.Data.ExprMapping
-- import           Luna.Builtin.Data.Module          (Imports (..), unionsImports)
-- import           Luna.Pass.Resolution.Data.CurrentTarget (CurrentTarget (TgtNone))
-- import qualified Luna.Pass.UnitCompilation.ModuleProcessing as ModuleTC
-- import qualified Luna.Pass.Sourcing.UnitLoader              as UnitLoader
-- import           Luna.IR.Term.World                         (WorldExpr)
-- import           Luna.IR.Term.Unit                          (UnitSet)


-- type PMStack m = PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger m)))

-- runPM :: MonadIO m => PMStack m a -> m a
-- runPM = dropLogs . DepState.evalDefStateT @Cache . evalIRBuilder' . evalPassManager'

-- type ASTOpReq a m = (MonadThrow m,
--                      MonadCatch m,
--                      MonadPassManager m,
--                      MonadIO m,
--                      MonadState a m,
--                      Emitters EmpireEmitters m,
--                      Editors Net  '[AnyExpr, AnyExprLink] m,
--                      Editors Attr '[Source, Parser.ParsedExpr, MarkedExprMap, Invalids] m,
--                      Editors Layer EmpireLayers m,
--                      DepOld.MonadGet Vis.V Vis.Vis m,
--                      DepOld.MonadPut Vis.V Vis.Vis m)

-- type ASTOpReq a m = (MonadThrow m,
--                      MonadCatch m,
--                      MonadIO m,
--                      MonadState a m,
--                      Layered.Getter (ByteSize (Component Edges)) m,
--                      Layered.Getter (ByteSize (Component Nodes)) m,
--                      Layered.Getter (Layer.DynamicManager Edges) m,
--                      Layered.Getter (Layer.DynamicManager Nodes) m,
--                      Layered.Getter (MemPool (Component.Some Edges)) m,
--                      Layered.Getter (MemPool (Component.Some Nodes)) m,
--                      Layered.Getter (LunaGraph.LayerByteOffset Nodes Meta) m,
--                      Layered.Getter (LunaGraph.LayerByteOffset Nodes Marker) m,
--                      Layered.Getter (LunaGraph.LayerByteOffset Edges SpanOffset) m,
--                      Layered.Getter (LunaGraph.LayerByteOffset Edges SpanOffset) m,
--                      Layer.Reader (Component Nodes) IR.Model m,
--                      Layer.Writer (Component Nodes) IR.Model m,
--                      Layer.Reader (Component Nodes) Marker m,
--                      Layer.Reader (Component Nodes) TypeLayer m,
--                      Layer.Reader (Component Nodes) IR.Users m,
--                      Layer.Reader (Component Nodes) CodeSpan m,
--                      Layer.Reader (Component Nodes) SpanLength m,
--                      Layer.Reader (Component Nodes) Meta m,
--                      Layer.Writer (Component Nodes) Meta m,
--                      Layer.Writer (Component Nodes) Marker m,
--                      Layer.Writer (Component Nodes) IR.Type m,
--                      Layer.Writer (Component Nodes) IR.Users m,
--                      Layer.Writer (Component Nodes) SpanLength m,
--                      Layer.Writer (Component Nodes) CodeSpan m,
--                      Layer.Reader (Component Edges) IR.Source m,
--                      Layer.Writer (Component Edges) IR.Source m,
--                      Layer.Reader (Component Edges) IR.Target m,
--                      Layer.Writer (Component Edges) IR.Target m,
--                      Layer.Reader (Component Edges) SpanOffset m,
--                      Layer.Writer (Component Edges) SpanOffset m,
--                      a ~ a)

type ASTOpReq a m = (MonadIO m,
                     MonadState a m,
                     MonadThrow m,
                     Layer.Reader (Component Nodes) IR.Model m,
                     Layer.Reader (Component Nodes) Marker m,
                     Layer.Writer (Component Nodes) Marker m,
                     Layer.Reader (Component Nodes) IR.Users m,
                     Layer.Reader (Component Nodes) CodeSpan m,
                     Layer.Writer (Component Nodes) CodeSpan m,
                     Layer.Reader (Component Nodes) SpanLength m,
                     Layer.Writer (Component Nodes) SpanLength m,
                     Layer.Reader (Component Nodes) Meta m,
                     Layer.Writer (Component Nodes) Meta m,
                     Layer.Reader (Component Nodes) TypeLayer m,
                     Layer.Reader (Component Edges) SpanOffset m,
                     Layer.Writer (Component Edges) SpanOffset m,
                     Layer.Reader IR.Link IR.Source m,
                     Layer.Reader IR.Link IR.Target m,
                     Layer.Writer Node IR.Type m,
                     Layer.Writer Node IR.Users m,
                     Layer.Writer Node IR.Model m,
                     Layer.Writer Edge IR.Target m,
                     Layer.Writer Edge IR.Source m,
                     Destruct.DeleteSubtree m,
                     -- LunaGraph.ComputeLayerByteOffset Source (LunaGraph.ComponentLayers EmpirePass Edges),
                     -- Layered.Getter (LunaGraph.LayerByteOffset Nodes SpanLength) m,
                     -- Layered.Getter (LunaGraph.LayerByteOffset Nodes Meta) m,
                     -- Layered.Getter (LunaGraph.LayerByteOffset Nodes Marker) m,
                     -- Layered.Getter (LunaGraph.LayerByteOffset Nodes CodeSpan) m,
                     Layered.Getter (ByteSize (Component Nodes)) m,
                     Layered.Getter (ByteSize (Component Edges)) m,
                     Layered.Getter (Layer.DynamicManager Edges) m,
                     Layered.Getter (Layer.DynamicManager Nodes) m,
                     Layered.Getter (MemPool (Component.Some Nodes)) m,
                     Layered.Getter (MemPool (Component.Some Edges)) m,
                     MonadCatch m)

type Printer a m = (MonadIO m,
                     MonadState a m,
                     MonadThrow m,
                     Layer.Reader (Component Nodes) IR.Model m,
                     Layer.Reader (Component Nodes) Marker m,
                     Layer.Writer (Component Nodes) Marker m,
                     Layer.Reader (Component Nodes) IR.Users m,
                     Layer.Reader (Component Nodes) CodeSpan m,
                     Layer.Writer (Component Nodes) CodeSpan m,
                     Layer.Reader (Component Nodes) SpanLength m,
                     Layer.Writer (Component Nodes) SpanLength m,
                     Layer.Reader (Component Nodes) Meta m,
                     Layer.Writer (Component Nodes) Meta m,
                     Layer.Reader (Component Nodes) TypeLayer m,
                     Layer.Reader (Component Edges) SpanOffset m,
                     Layer.Writer (Component Edges) SpanOffset m,
                     Layer.Reader IR.Link IR.Source m,
                     Layer.Reader IR.Link IR.Target m,
                     Layer.Writer Node IR.Type m,
                     Layer.Writer Node IR.Users m,
                     Layer.Writer Node IR.Model m,
                     Layer.Writer Edge IR.Target m,
                     Layer.Writer Edge IR.Source m,
                     -- LunaGraph.ComputeLayerByteOffset Source (LunaGraph.ComponentLayers EmpirePass Edges),
                     -- Layered.Getter (LunaGraph.LayerByteOffset Nodes SpanLength) m,
                     -- Layered.Getter (LunaGraph.LayerByteOffset Nodes Meta) m,
                     -- Layered.Getter (LunaGraph.LayerByteOffset Nodes Marker) m,
                     -- Layered.Getter (LunaGraph.LayerByteOffset Nodes CodeSpan) m,
                     Layered.Getter (ByteSize (Component Nodes)) m,
                     Layered.Getter (ByteSize (Component Edges)) m,
                     Layered.Getter (Layer.DynamicManager Edges) m,
                     Layered.Getter (Layer.DynamicManager Nodes) m,
                     Layered.Getter (MemPool (Component.Some Nodes)) m,
                     Layered.Getter (MemPool (Component.Some Edges)) m,
                     MonadCatch m)
                     -- Emitters EmpireEmitters m,
                     -- Editors Net  '[AnyExpr, AnyExprLink] m,
                     -- Editors Attr '[Source, Parser.ParsedExpr, MarkedExprMap, Invalids] m,
                     -- Editors Layer EmpireLayers m)
                     -- DepOld.MonadGet Vis.V Vis.Vis m,
                     -- DepOld.MonadPut Vis.V Vis.Vis m)

type instance LunaGraph.Discover (StateT s m) = LunaGraph.Discover m
type instance LunaGraph.Discover (Layered.StateT s m) = LunaGraph.Discover m


type ASTOp a m = (ASTOpReq a m, HasCallStack)
type GraphOp m = ASTOp Graph m
type ClassOp m = ASTOp ClsGraph m


-- type EmpireLayers = '[AnyExpr // Model, AnyExprLink // Model,
--                       AnyExpr // Marker,
--                       AnyExpr // Meta,
--                       AnyExpr // Succs,
--                       AnyExpr // Errors,
--                       AnyExpr // TypeLayer,
--                       AnyExpr // UID, AnyExprLink // UID,
--                       AnyExpr // SpanLength,
--                       AnyExprLink // SpanOffset,
--                       AnyExpr // CodeSpan.CodeSpan]

-- type EmpireEmitters = '[New // AnyExpr, New // AnyExprLink,
--                         Import // AnyExpr, Import // AnyExprLink,
--                         Delete // AnyExpr, Delete // AnyExprLink,
--                         OnDeepDelete // AnyExpr]

newtype PassReturnValue = PassReturnValue (Maybe Any)
type instance Attr.Type PassReturnValue = Attr.Atomic
instance Default PassReturnValue where def = PassReturnValue Nothing

data EmpirePass
type instance Pass.Spec EmpirePass t = EmpirePassSpec t
type family EmpirePassSpec t where
    EmpirePassSpec (Pass.In  Pass.Attrs)  = '[]  -- Parser attrs temporarily - probably need to call it as a separate Pass
    EmpirePassSpec (Pass.Out Pass.Attrs)  = '[]
    EmpirePassSpec (Pass.In  AnyExpr)     = '[Model, Type, Users, SpanLength, CodeSpan, Meta, Marker]
    EmpirePassSpec (Pass.Out AnyExpr)     = '[Model, Type, Users, SpanLength, CodeSpan, Meta, Marker]
    EmpirePassSpec (Pass.In  AnyExprLink) = '[SpanOffset, Source, Target]
    EmpirePassSpec (Pass.Out AnyExprLink) = '[SpanOffset, Source, Target]
    EmpirePassSpec t                      = Pass.BasicPassSpec t


-- Pass.cache_phase1 ''EmpirePass
-- Pass.cache_phase2 ''EmpirePass

-- deriving instance MonadMask m => MonadMask (Pass.PassManager m)
-- deriving instance MonadMask m => MonadMask (DepState.StateT t m)
-- deriving instance MonadMask m => MonadMask (Logger (IdentityLogger l) m)

-- instance MonadPassManager m => MonadRefLookup Net (Pass.SubPass pass m) where
--     uncheckedLookupRef = lift . uncheckedLookupRef

-- instance MonadPassManager m => MonadRefLookup Event (Pass.SubPass pass m) where
--     uncheckedLookupRef = lift . uncheckedLookupRef

-- instance MonadPassManager m => MonadRefLookup Layer (Pass.SubPass pass m) where
--     uncheckedLookupRef = lift . uncheckedLookupRef

-- instance MonadPassManager m => MonadRefLookup Attr (Pass.SubPass pass m) where
--     uncheckedLookupRef = lift . uncheckedLookupRef

match :: (Monad m, Layer.Reader t Model m) => t layout -> (UniTerm layout -> m a) -> m a
match = matchExpr
deriving instance MonadCatch (Pass stage t)
deriving instance MonadCatch m => MonadCatch (MultiState.MultiStateT s m)
deriving instance MonadCatch m => MonadCatch (LunaGraph.GraphT s m)
deriving instance MonadCatch m => MonadCatch (Layered.StateT s m)
-- deriving instance MonadCatch m => MonadCatch (Pass.PassManager m)

-- class GraphRunner g where
--     runPass :: forall pass b a. KnownPass pass
--             => Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT g IO))))) b
--             -> Pass.SubPass pass (Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT g IO)))))) a
--             -> Command g a

-- instance GraphRunner Graph where
--     runPass :: forall pass b a. KnownPass pass
--             => Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT Graph IO))))) b
--             -> Pass.SubPass pass (Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT Graph IO)))))) a
--             -> Command Graph a
--     runPass inits pass = do
--         g <- get
--         AST currentStateIR currentStatePass <- use Graph.ast
--         let evalIR = flip runStateT g
--                    . withVis
--                    . dropLogs
--                    . DepState.evalDefStateT @Cache
--                    . flip evalIRBuilder currentStateIR
--                    . flip evalPassManager currentStatePass
--         ((a, (st, passSt)), newG) <- liftIO $ evalIR $ do
--             inits
--             a      <- Pass.eval' @pass pass
--             st     <- snapshot
--             passSt <- DepState.get @Pass.State
--             return (a, (st, passSt))
--         put $ newG & Graph.ast .~ AST st passSt

--         return a

-- instance GraphRunner ClsGraph where
--     runPass :: forall pass b a. KnownPass pass
--             => Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT ClsGraph IO))))) b
--             -> Pass.SubPass pass (Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT ClsGraph IO)))))) a
--             -> Command ClsGraph a
--     runPass inits pass = do
--         g <- get
--         AST currentStateIR currentStatePass <- use Graph.clsAst
--         let evalIR = flip runStateT g
--                    . withVis
--                    . dropLogs
--                    . DepState.evalDefStateT @Cache
--                    . flip evalIRBuilder currentStateIR
--                    . flip evalPassManager currentStatePass
--         ((a, (st, passSt)), newG) <- liftIO $ evalIR $ do
--             inits
--             a      <- Pass.eval' @pass pass
--             st     <- snapshot
--             passSt <- DepState.get @Pass.State
--             return (a, (st, passSt))
--         put $ newG & Graph.clsAst .~ AST st passSt

--         return a


defaultClsAST :: IO (IR.SomeTerm, LunaGraph.State PatternTransformation.EmpireStage, Scheduler.State)
defaultClsAST = do
    ((a, scState), grState) <- LunaGraph.encodeAndEval @PatternTransformation.EmpireStage $ do
        foo <- Scheduler.runT $ do
            -- Scheduler.registerAttr     @PassReturnValue
            -- Scheduler.enableAttrByType @PassReturnValue
            m <- liftIO $ newIORef (error "emptyreturn")
            Scheduler.registerPassFromFunction__ @PatternTransformation.EmpireStage @EmpirePass $ do
                a <- do
                    hub <- IR.importHub []
                    c <- IR.record False "A" [] [] []
                    IR.unit hub [] c
                liftIO $ writeIORef m $ generalize a
                -- Attr.put $ PassReturnValue $ unsafeCoerce $ Just a
                -- b <- Attr.get @PassReturnValue
                -- b <- liftIO $ readIORef m
                -- print a
                -- print b
                -- matchExpr b $ \case
                --     Unit _ _ c -> print =<< source c
            Scheduler.runPassByType @EmpirePass
            -- st <- Layered.get @Scheduler.State
            -- let [a] = Map.elems (st ^. Scheduler.attrs)
            -- putStrLn "BOOM"
            -- let Just (PassReturnValue foo) = unsafeCoerce a
            -- putStrLn "BOOMBOX"
            -- let p = (unsafeCoerce foo :: IR.SomeTerm)
            -- print p
            -- ret <- Scheduler.lookupAttr @PassReturnValue
            -- case ret of
            --     Nothing -> error "runASTOp: pass didn't register return value"
            --     Just (PassReturnValue foo) -> case unsafeCoerce foo of
            --         Just a -> return a
            --         _      -> error "default: empty return"
            foo <- liftIO $ readIORef m
            -- st <- Layered.get @Scheduler.State
            return foo
        st <- LunaGraph.getState
        return (foo, st)
    -- print "returned"
    return (a, grState, scState)

defaultPMState :: IO (Graph.PMState a)
defaultPMState = do
    (scState, grState) <- LunaGraph.encodeAndEval @PatternTransformation.EmpireStage $ do
        ((), foo) <- Scheduler.runT $ return ()
        st <- LunaGraph.getState
        return (foo, st)
    return $ Graph.PMState scState grState (error "PMState graph")


defaultClsGraph :: IO ClsGraph
defaultClsGraph = do
    (ast, scSt, grSt) <- defaultClsAST
    let cls = Graph.ClsGraph (AST () (Graph.PMState grSt scSt (error "g"))) ast def def def def def
    return cls


runASTOp :: forall a g m. (Graph.HasAST g)
         => (StateT g (Pass.Pass PatternTransformation.EmpireStage EmpirePass) a)
         -> Command g a
runASTOp p = do
    g <- get
    m <- liftIO $ newIORef (error "emptyreturn")

    (((g', ret), scSt), grSt) <- flip LunaGraph.run (g ^. Graph.ast . Graph.pmState . Graph.pmStage) $ flip Layered.runT (g ^. Graph.ast . Graph.pmState . Graph.pmScheduler) $ unwrap $ Scheduler.SchedulerT $ do
        -- Scheduler.registerAttr     @PassReturnValue
        -- Scheduler.enableAttrByType @PassReturnValue
        Scheduler.registerPassFromFunction__ @PatternTransformation.EmpireStage @EmpirePass $ do
            (a, g') <- runStateT p g
            -- liftIO $ print "put Ret1"
            -- Attr.put $ PassReturnValue $ unsafeCoerce $ Just (g', a)
            liftIO $ writeIORef m (g', a)
            -- liftIO $ print "put Ret2"
        Scheduler.runPassByType @EmpirePass
        (g', a) <- liftIO $ readIORef m
        return (g', a)
        -- ret <- Scheduler.lookupAttr @PassReturnValue
        -- case ret of
        --     Nothing -> error "runASTOp: pass didn't register return value"
        --     Just (PassReturnValue foo) -> case unsafeCoerce foo of
        --         Just a -> return a
        --         _      -> error "runastop: empty return"
    -- print "runned"
    -- put g'
    put $ g' & Graph.ast . Graph.pmState . Graph.pmScheduler .~ scSt
    put $ g' & Graph.ast . Graph.pmState . Graph.pmStage .~ grSt
    -- print "putted"
    return ret

runPass :: forall pass a g. (Graph.HasAST g, Typeable pass, _)
         => _
         -> (StateT g (Pass.Pass PatternTransformation.EmpireStage pass) a)
         -> Command g a
runPass inits p = do
    g <- get
    m <- liftIO $ newIORef (error "emptyreturn")

    (((g', ret), scSt), grSt) <- flip LunaGraph.run (g ^. Graph.ast . Graph.pmState . Graph.pmStage) $ flip Layered.runT (g ^. Graph.ast . Graph.pmState . Graph.pmScheduler) $ unwrap $ Scheduler.SchedulerT $ do
        -- Scheduler.registerAttr     @PassReturnValue
        -- Scheduler.enableAttrByType @PassReturnValue
        inits
        Scheduler.registerPassFromFunction__ @PatternTransformation.EmpireStage @pass $ do
            (a, g') <- runStateT p g
            -- liftIO $ print "put Ret1"
            -- Attr.put $ PassReturnValue $ unsafeCoerce $ Just (g', a)
            liftIO $ writeIORef m (g', a)
            -- liftIO $ print "put Ret2"
        Scheduler.runPassByType @pass
        (g', a) <- liftIO $ readIORef m
        return (g', a)
        -- ret <- Scheduler.lookupAttr @PassReturnValue
        -- case ret of
        --     Nothing -> error "runASTOp: pass didn't register return value"
        --     Just (PassReturnValue foo) -> case unsafeCoerce foo of
        --         Just a -> return a
        --         _      -> error "runastop: empty return"
    -- print "runned"
    -- put g'
    put $ g' & Graph.ast . Graph.pmState . Graph.pmScheduler .~ scSt
    put $ g' & Graph.ast . Graph.pmState . Graph.pmStage .~ grSt
    -- print "putted"
    return ret

    -- inits = do
    --     setAttr @MarkedExprMap $ (mempty :: MarkedExprMap)
    --     setAttr @Invalids      $ (mempty :: Invalids)
    --     setAttr @Source        $ (error "Data not provided: Source")
        -- setAttr (getTypeDesc @Parser.ParsedExpr) $ (error "Data not provided: ParsedExpr")


runAliasAnalysis :: Command Graph ()
runAliasAnalysis = do
    root <- use $ Graph.breadcrumbHierarchy . BH.self
    let inits = do
            Scheduler.registerAttr @Root
            Scheduler.enableAttrByType @Root
            Scheduler.setAttr @Root $ Root root
            Scheduler.registerAttr @PatternTransformation.ExprRoots
            Scheduler.enableAttrByType @PatternTransformation.ExprRoots
            Scheduler.setAttr @PatternTransformation.ExprRoots $ PatternTransformation.ExprRoots [coerce root]
            Scheduler.registerAttr @UnresolvedVariables
            Scheduler.enableAttrByType @UnresolvedVariables
    runPass @PatternTransformation.PatternTransformation inits $ StateT $ \a -> PatternTransformation.runPatternTransformation >> return ((), a)
    runPass @AliasAnalysis.AliasAnalysis inits $ StateT $ \a -> Pass.definition @PatternTransformation.EmpireStage @AliasAnalysis.AliasAnalysis >> return ((), a)
      -- -- let inits = do
      -- --         Pass.setAttr (getTypeDesc @UnresolvedVars)   $ UnresolvedVars   []
      -- --         Pass.setAttr (getTypeDesc @UnresolvedConses) $ UnresolvedConses []
      -- --         Pass.setAttr (getTypeDesc @NegativeConses)   $ NegativeConses   []
      -- --         Pass.setAttr (getTypeDesc @ExprRoots) $ ExprRoots [unsafeGeneralize root]
      -- -- runPass inits PatternTransformation.runPatternTransformation
      -- -- runASTOp AliasAnalysis.runAliasAnalysis

-- runTypecheck :: CurrentTarget -> Imports -> Command Graph ()
-- runTypecheck currentTarget imports = do
--     g <- get
--     AST currentStateIR currentStatePass <- use Graph.ast
--     root <- use $ Graph.breadcrumbHierarchy . BH.self
--     let evalIR = flip runStateT g
--                . withVis
--                . dropLogs
--                . DepState.evalDefStateT @Cache
--                . flip evalIRBuilder currentStateIR
--                . flip evalPassManager currentStatePass
--     ((st, passSt), newG) <- liftIO $ evalIR $ do
--         Typecheck.typecheck currentTarget imports [unsafeGeneralize root]
--         st     <- snapshot
--         passSt <- DepState.get @Pass.State
--         return (st, passSt)
--     put $ newG & Graph.ast .~ AST st passSt

-- evalTC :: ClsGraph
--        -> IR
--        -> State (Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT ClsGraph IO))))))
--        -> Pass.PassManager (IRBuilder (DepState.StateT Cache (Logger DropLogger (Vis.VisStateT (StateT ClsGraph IO))))) a
--        -> IO (a, ClsGraph)
-- evalTC g ir pmState = flip runStateT g
--                     . withVis
--                     . dropLogs
--                     . DepState.evalDefStateT @Cache
--                     . flip evalIRBuilder ir
--                     . flip evalPassManager pmState

-- tcInit :: MonadPassManager m => m ()
-- tcInit = do
--     Pass.setAttr (getTypeDesc @WorldExpr)                 $ error "Data not provided: WorldExpr"
--     Pass.setAttr (getTypeDesc @UnitLoader.UnitsToLoad)    $ error "Data not provided: UnitsToLoad"
--     Pass.setAttr (getTypeDesc @UnitLoader.SourcesManager) $ error "Data not provided: SourcesManager"
--     Pass.setAttr (getTypeDesc @UnitSet)                   $ error "Data not provided: UnitSet"
--     Pass.setAttr (getTypeDesc @Invalids)                  $ (mempty :: Invalids)
--     initNameGen

-- extractImportedModules :: (MonadPassManager m, MonadException PassEvalError m) => Expr Unit -> m (Set.Set QualName)
-- extractImportedModules unit = do
--     impNames <- Pass.eval' $ do
--         imphub   <- unit @^. Term.imports
--         imps     <- readWrappedSources (unsafeGeneralize imphub :: Expr (UnresolvedImportHub >> UnresolvedImport >> UnresolvedImportSrc))
--         impNames <- for imps $ \imp -> do
--             src <- imp @^. Term.termUnresolvedImport_source
--             Term.Absolute path <- src @. wrapped
--             return path
--         cls <- IR.matchExpr unit $ \case
--             IR.Unit _ _ c -> IR.source c
--         UnitLoader.partitionASGCls $ IR.unsafeGeneralize cls
--         return impNames
--     let impNamesWithBase = Set.insert "Std.Base" $ Set.fromList impNames
--     return impNamesWithBase

-- getImportedModules :: Command ClsGraph (Set.Set QualName)
-- getImportedModules = do
--     unit :: Expr Unit <- uses Graph.clsClass unsafeGeneralize
--     g <- get
--     AST ir pmState <- use Graph.clsAst
--     fst <$> liftIO (evalTC g ir pmState $ tcInit >> extractImportedModules unit)

-- runModuleTypecheck :: QualName -> Map.Map Name FilePath -> CompiledModules -> Command ClsGraph (Either Compilation.ModuleCompilationError (Imports, CompiledModules))
-- runModuleTypecheck moduleName sources cmpMods@(CompiledModules _ prims) = do
--     unit :: Expr Unit <- uses Graph.clsClass unsafeGeneralize
--     g <- get
--     AST ir pmState <- use Graph.clsAst
--     (res, newG) <- liftIO $ evalTC g ir pmState $ do
--         tcInit
--         impNamesWithBase <- Set.toList <$> extractImportedModules unit
--         result <- liftIO $ Compilation.requestModules sources impNamesWithBase cmpMods
--         case result of
--             Right (imports, newCmpMods) -> do
--                 let imps = unionsImports $ prims : Map.elems imports
--                 res <- ModuleTC.processModule imps (convert moduleName) (IR.unsafeGeneralize unit)
--                 return $ Right (res, newCmpMods)
--             Left err -> return $ Left err
--     return res

-- putNewIR :: IR -> Command Graph ()
-- putNewIR ir = do
--     pmState <- liftIO $ Graph.defaultPMState
--     Graph.ast .= AST ir pmState

-- putNewIRCls :: IR -> Command ClsGraph ()
-- putNewIRCls ir = do
--     newAST <- liftIO $ Graph.emptyClsAST
--     Graph.clsAst .= (newAST & Graph.ir .~ ir)

