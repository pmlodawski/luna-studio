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
  -- , PMStack
  -- , getImportedModules
  -- , putNewIR
  -- , putNewIRCls
  -- , runAliasAnalysis
  , runASTOp
  , runPass
  -- , runPM
  -- , runTypecheck
  -- , runModuleTypecheck
  , match
  ) where

import           Empire.Prelude       hiding (Type, mempty, toList)
import           Prologue             (mempty)

import           Control.Monad.Catch  (MonadCatch(..))
-- import           Control.Monad.Raise  (MonadException)
import           Control.Monad.State  (MonadState, StateT, runStateT, get, put)
-- import qualified Control.Monad.State.Dependent as DepState
import qualified Control.Monad.State.Layered as Layered
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Empire.Data.Graph    (AST(..), ClsGraph, Graph, runPass, withVis)
import qualified Empire.Data.Graph    as Graph
import qualified Empire.Data.BreadcrumbHierarchy as BH
import           Empire.Data.Layers   (Marker, Meta, TypeLayer, SpanLength, SpanOffset)
import           Empire.Empire        (Command)
import           GHC.Exts             (Any)
import           Unsafe.Coerce        (unsafeCoerce)

import qualified Luna.Pass        as Pass
import           Luna.IR              as IR hiding (Marker, match)
-- import qualified Luna.IR.Term.Unit    as Term
-- import           OCI.IR.Layout.Typed  (type (>>))
import Foreign.Info.ByteSize (ByteSize)
import Foreign.Memory.Pool (MemPool)
import           Data.Graph.Component   (Component, SomeComponent)
import qualified Data.Graph.Component.Layer   as Layer
-- import           OCI.IR.Name.Qualified (QualName)
import           OCI.Pass.Definition       (DataGetter, Pass(..)) --, ComponentSize, LayerMemManager, ComponentMemPool)
-- import           OCI.Pass.Class       (Inputs, Outputs, Preserves, KnownPass)
-- import           OCI.IR.Class         (Import)
-- import qualified OCI.Pass.Class       as Pass (SubPass, eval')
-- import qualified OCI.Pass.Manager     as Pass (PassManager, setAttr, State)
import qualified Luna.Runner as Runner
import qualified Luna.Pass.Scheduler as Scheduler
import qualified Luna.Pass.Attr as Attr

-- import           System.Log                                   (DropLogger(..), dropLogs)
-- import           System.Log.Logger.Class                      (Logger(..), IdentityLogger(..))
-- import           Luna.Pass.Data.ExprRoots                     (ExprRoots(..))
import qualified Empire.Pass.PatternTransformation            as PatternTransformation
-- import           Luna.Pass.Resolution.Data.UnresolvedVars     (UnresolvedVars(..))
-- import           Luna.Pass.Resolution.Data.UnresolvedConses   (UnresolvedConses(..), NegativeConses(..))
-- import qualified Luna.Pass.Resolution.AliasAnalysis           as AliasAnalysis
import           Luna.Syntax.Text.Parser.Data.Invalid (Invalids)
-- import qualified Luna.Syntax.Text.Parser.Parser               as Parser
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

type ASTOpReq a m = (MonadThrow m,
                     MonadCatch m,
                     -- MonadPassManager m,
                     MonadIO m,
                     MonadState a m,
                     Layered.Getter (ByteSize (Component IR.Links)) m,
                     Layered.Getter (ByteSize (Component IR.Terms)) m,
                     Layered.Getter (Layer.DynamicManager IR.Links) m,
                     Layered.Getter (Layer.DynamicManager IR.Terms) m,
                     Layered.Getter (MemPool (SomeComponent IR.Links)) m,
                     Layered.Getter (MemPool (SomeComponent IR.Terms)) m,
                     -- DataGetter (LayerMemManager IR.Links) m,
                     -- DataGetter (LayerMemManager IR.Terms) m,
                     -- DataGetter (ComponentMemPool IR.Links) m,
                     -- DataGetter (ComponentMemPool IR.Terms) m,
                     -- DataGetter (ComponentSize IR.Links) m,
                     -- DataGetter (ComponentSize IR.Terms) m,
                     Layer.Reader IR.Term IR.Model m,
                     Layer.Writer IR.Term IR.Model m,
                     Layer.Reader IR.Term Marker m,
                     Layer.Reader IR.Term TypeLayer m,
                     Layer.Reader IR.Term IR.Users m,
                     Layer.Reader IR.Term CodeSpan m,
                     Layer.Reader IR.Term SpanLength m,
                     Layer.Reader IR.Term Meta m,
                     Layer.Writer IR.Term Meta m,
                     Layer.Writer IR.Term Marker m,
                     Layer.Writer IR.Term IR.Type m,
                     Layer.Writer IR.Term IR.Users m,
                     Layer.Writer IR.Term SpanLength m,
                     Layer.Writer IR.Term CodeSpan m,
                     Layer.Reader IR.Link IR.Source m,
                     Layer.Writer IR.Link IR.Source m,
                     Layer.Reader IR.Link IR.Target m,
                     Layer.Writer IR.Link IR.Target m,
                     Layer.Reader IR.Link SpanOffset m,
                     Layer.Writer IR.Link SpanOffset m)
                     -- Emitters EmpireEmitters m,
                     -- Editors Net  '[AnyExpr, AnyExprLink] m,
                     -- Editors Attr '[Source, Parser.ParsedExpr, MarkedExprMap, Invalids] m,
                     -- Editors Layer EmpireLayers m)
                     -- DepOld.MonadGet Vis.V Vis.Vis m,
                     -- DepOld.MonadPut Vis.V Vis.Vis m)

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

newtype PassReturnValue = PassReturnValue Any
type instance Attr.Type PassReturnValue = Attr.Atomic
instance Default PassReturnValue where def = PassReturnValue (error "empty PassReturnValue")

data EmpirePass
type instance Pass.Spec EmpirePass t = EmpirePassSpec t
type family EmpirePassSpec t where
    EmpirePassSpec (Pass.In  Pass.Attrs)  = '[]  -- Parser attrs temporarily - probably need to call it as a separate Pass
    EmpirePassSpec (Pass.Out Pass.Attrs)  = '[PassReturnValue]
    EmpirePassSpec (Pass.In  AnyExpr)     = '[Model, Type, Users, Meta, Marker, SpanLength, CodeSpan]
    EmpirePassSpec (Pass.Out AnyExpr)     = '[Model, Type, Users, Meta, Marker, SpanLength, CodeSpan]
    EmpirePassSpec (Pass.In  AnyExprLink) = '[SpanOffset, Source, Target]
    EmpirePassSpec (Pass.Out AnyExprLink) = '[SpanOffset, Source, Target]
    EmpirePassSpec t                      = Pass.BasicPassSpec t


Pass.cache_phase1 ''EmpirePass
Pass.cache_phase2 ''EmpirePass

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

match = matchExpr
deriving instance MonadCatch (Pass t)
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

runASTOp :: forall a g. ()
         => StateT g (Pass.Pass EmpirePass) a
         -> Command g a
runASTOp p = do
    g <- get
    (g', a) <- Runner.runManual $ do
        Scheduler.registerAttr     @PassReturnValue
        Scheduler.enableAttrByType @PassReturnValue
        Scheduler.registerPassFromFunction__ $ do
            (g', a) <- runStateT p g
            Attr.put $ PassReturnValue $ unsafeCoerce (g', a)
        ret <- Scheduler.lookupAttr @PassReturnValue
        case ret of
            Nothing -> error "runASTOp: pass didn't register return value"
            Just (PassReturnValue a) -> return $ unsafeCoerce a
    put g'
    return a

    -- inits = do
    --     setAttr @MarkedExprMap $ (mempty :: MarkedExprMap)
    --     setAttr @Invalids      $ (mempty :: Invalids)
    --     setAttr @Source        $ (error "Data not provided: Source")
        -- setAttr (getTypeDesc @Parser.ParsedExpr) $ (error "Data not provided: ParsedExpr")


runAliasAnalysis :: Command Graph ()
runAliasAnalysis = return () -- do
    -- root <- use $ Graph.breadcrumbHierarchy . BH.self
    -- let inits = do
    --         Pass.setAttr (getTypeDesc @UnresolvedVars)   $ UnresolvedVars   []
    --         Pass.setAttr (getTypeDesc @UnresolvedConses) $ UnresolvedConses []
    --         Pass.setAttr (getTypeDesc @NegativeConses)   $ NegativeConses   []
    --         Pass.setAttr (getTypeDesc @ExprRoots) $ ExprRoots [unsafeGeneralize root]
    -- runPass inits PatternTransformation.runPatternTransformation
    -- runPass inits AliasAnalysis.runAliasAnalysis

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

