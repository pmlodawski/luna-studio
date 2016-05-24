module Empire.Commands.Typecheck where

import           Prologue
import           Control.Monad.State
import           Unsafe.Coerce           (unsafeCoerce)
import           Control.Monad.Error     (throwError)
import           Control.Monad           (forM, forM_)
import           Data.IntMap             (IntMap)
import qualified Data.IntMap             as IntMap
import           Data.List               (sort)
import qualified Data.Map                as Map
import           Data.Maybe              (isNothing, fromMaybe)

import qualified Empire.Data.Graph       as Graph
import           Empire.Data.Graph       (Graph)

import           Empire.API.Data.Node              (NodeId)
import           Empire.API.Data.DefaultValue      (Value (..))
import           Empire.API.Data.GraphLocation     (GraphLocation (..))
import qualified Empire.API.Graph.NodeResultUpdate as NodeResult

import           Empire.Empire
import qualified Empire.Commands.AST          as AST
import qualified Empire.Commands.GraphUtils   as GraphUtils
import qualified Empire.Commands.GraphBuilder as GraphBuilder
import qualified Empire.Commands.Publisher    as Publisher

import qualified StdLibMock                                      as StdLib
import qualified Luna.Library.Symbol                             as Symbol
import qualified Luna.Compilation.Stage.TypeCheck                as TypeCheck
import qualified Luna.Compilation.Stage.TypeCheck.Class          as TypeCheckState
import           Luna.Compilation.Stage.TypeCheck                (Loop (..), Sequence (..))
import           Luna.Compilation.Pass.Inference.Literals        (LiteralsPass (..))
import           Luna.Compilation.Pass.Inference.Struct          (StructuralInferencePass (..))
import           Luna.Compilation.Pass.Inference.Unification     (StrictUnificationPass (..))
import           Luna.Compilation.Pass.Inference.Calling         (FunctionCallingPass (..))
import           Luna.Compilation.Pass.Inference.Importing       (SymbolImportingPass (..))
import           Luna.Compilation.Pass.Inference.Scan            (ScanPass (..))
import           Luna.Syntax.Model.Network.Builder               (Sign (..))

import qualified Luna.Compilation.Pass.Interpreter.Interpreter   as Interpreter

import qualified Empire.ASTOp as ASTOp
import           Empire.Data.AST                                 (AST, NodeRef)

getNodeValue :: NodeId -> Command Graph (Maybe Value)
getNodeValue nid = do
    ref <- GraphUtils.getASTTarget nid
    zoom Graph.ast $ AST.getNodeValue ref

collect pass = return ()
    {-putStrLn $ "After pass: " <> pass-}
    {-st <- TypeCheckState.get-}
    {-putStrLn $ "State is: " <> show st-}

runTC :: Command Graph ()
runTC = do
    allNodeIds <- uses Graph.nodeMapping Map.keys
    roots <- mapM GraphUtils.getASTPointer allNodeIds
    ast   <- use Graph.ast
    (_, g) <- TypeCheck.runT $ flip ASTOp.runGraph ast $ do
        Symbol.loadFunctions StdLib.symbols
        TypeCheckState.modify_ $ (TypeCheckState.freshRoots .~ roots)
        let seq3 a b c = Sequence a $ Sequence b c
            seq4 a b c d = Sequence a $ seq3 b c d
            seq5 a b c d e = Sequence a $ seq4 b c d e
        let tc = (seq4
                     ScanPass
                     LiteralsPass
                     StructuralInferencePass
                     (Loop $ Sequence
                         (Loop $ seq4
                             SymbolImportingPass
                             (Loop $ StrictUnificationPass Positive False)
                             FunctionCallingPass
                             (Loop $ StrictUnificationPass Positive False))
                         (StrictUnificationPass Negative True)))
        TypeCheck.runTCWithArtifacts tc collect
    Graph.ast .= g
    return ()

runInterpreter :: Command Graph ()
runInterpreter = do
    ast        <- use Graph.ast
    allNodes   <- uses Graph.nodeMapping Map.keys
    refs       <- mapM GraphUtils.getASTPointer allNodes
    metas      <- zoom Graph.ast $ mapM AST.readMeta refs
    let sorted = fmap snd $ sort $ zip metas allNodes
    evals      <- mapM GraphUtils.getASTVar sorted
    newAst     <- liftIO $ fmap snd $ flip ASTOp.runBuilder ast $ Interpreter.run evals
    Graph.ast .= newAst
    return ()

updateNodes :: GraphLocation -> Command InterpreterEnv ()
updateNodes loc = do
    allNodeIds <- uses (graph . Graph.nodeMapping) Map.keys
    forM_ allNodeIds $ \id -> do
        ref <- zoom graph $ GraphUtils.getASTTarget id

        err <- zoom (graph . Graph.ast) $ AST.getError ref
        cachedErr <- uses errorsCache $ Map.lookup id
        if cachedErr /= err
            then do
                errorsCache %= Map.alter (const err) id
                valuesCache %= Map.delete id
                case err of
                    Just e  -> Publisher.notifyResultUpdate loc id (NodeResult.Error e) 0
                    Nothing -> Publisher.notifyResultUpdate loc id (NodeResult.NoValue) 0
            else return ()

        rep <- zoom graph $ GraphBuilder.buildNode id
        cached <- uses nodesCache $ Map.lookup id
        if cached /= Just rep
            then do
                Publisher.notifyNodeUpdate loc rep
                nodesCache %= Map.insert id rep
            else return ()


updateValues :: GraphLocation -> Command InterpreterEnv ()
updateValues loc = do
    allNodeIds <- uses (graph . Graph.nodeMapping) Map.keys
    forM_ allNodeIds $ \id -> do
        noErrors <- isNothing <$> uses errorsCache (Map.lookup id)
        if noErrors
            then do
                val    <- zoom graph $ getNodeValue id
                cached <- uses valuesCache $ Map.lookup id
                if cached /= Just val
                    then do
                        Publisher.notifyResultUpdate loc id (fromMaybe NodeResult.NoValue $ NodeResult.Value <$> val) 100
                        valuesCache %= Map.insert id val
                    else return ()
            else return ()

flushCache :: Command InterpreterEnv ()
flushCache = do
    errorsCache .= def
    valuesCache .= def
    nodesCache  .= def

run :: GraphLocation -> Command InterpreterEnv ()
run loc = do
    zoom graph runTC
    updateNodes loc
    zoom graph runInterpreter
    updateValues loc
