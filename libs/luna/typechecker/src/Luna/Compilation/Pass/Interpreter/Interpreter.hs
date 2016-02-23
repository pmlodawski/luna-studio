{-# LANGUAGE CPP                       #-}

module Luna.Compilation.Pass.Interpreter.Interpreter where

import           Prologue                                        hiding (Getter, Setter, pre, read, succ, ( # ))

import           Control.Monad                                   (forM_)
import           Control.Monad.Event                             (Dispatcher)
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.State
import           Data.Construction
import           Data.Graph
import           Data.Graph.Backend.VectorGraph
import           Data.Graph.Builder
import qualified Data.IntSet                                     as IntSet
import           Data.Prop
import           Data.Record                                     hiding (cons)
import           Development.Placeholders

import           Luna.Compilation.Pass.Interpreter.Env           (Env)
import qualified Luna.Compilation.Pass.Interpreter.Env           as Env
import           Luna.Compilation.Pass.Interpreter.Class         (InterpreterMonad, InterpreterT, runInterpreterT)
import           Luna.Compilation.Pass.Interpreter.Layer         (InterpreterData (..), InterpreterLayer)
import qualified Luna.Compilation.Pass.Interpreter.Layer         as Layer

import           Luna.Evaluation.Runtime                         (Dynamic, Static)
import           Luna.Syntax.AST.Term.Class                      (Lam)
import           Luna.Syntax.Builder
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Luna.Syntax.Model.Network.Term

import           Type.Inference



#define InterpreterCtx(m, ls, term) ( ls   ~ NetLayers a                                        \
                                    , term ~ Draft Static                                       \
                                    , ne   ~ Link (ls :<: term)                                 \
                                    , BiCastable e ne                                           \
                                    , BiCastable n (ls :<: term)                                \
                                    , MonadIO m                                                 \
                                    , MonadBuilder (Hetero (VectorGraph n e c)) m               \
                                    , NodeInferable m (ls :<: term)                             \
                                    , TermNode Lam  m (ls :<: term)                             \
                                    , HasProp InterpreterData (ls :<: term)                     \
                                    , Prop    InterpreterData (ls :<: term) ~ InterpreterLayer  \
                                    , InterpreterMonad (Env (Ref Node (ls :<: term))) m         \
                                    )



pre :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m [Ref Node (ls :<: term)]
pre ref = do
    node <- read ref
    mapM (follow target) $ node # Inputs

succ :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m [Ref Node (ls :<: term)]
succ ref = do
    node <- read ref
    mapM (follow source) $ node # Succs

isDirty :: (Prop InterpreterData n ~ InterpreterLayer, HasProp InterpreterData n) => n -> Bool
isDirty node = (node # InterpreterData) ^. Layer.dirty

isRequired :: (Prop InterpreterData n ~ InterpreterLayer, HasProp InterpreterData n) => n -> Bool
isRequired node = (node # InterpreterData) ^. Layer.required

markDirty :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m ()
markDirty ref = do
    node <- read ref
    write ref (node & prop InterpreterData . Layer.dirty .~ True)

followDirty :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m ()
followDirty ref = do
    Env.addReqNode ref
    prevs <- pre ref
    forM_ prevs $ \p -> do
        nd <- read p
        whenM (isDirty <$> read p) $
            followDirty p

markSuccessors :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m ()
markSuccessors ref = do
    node <- read ref
    unless (isDirty node) $ do
        markDirty ref
        when (isRequired node) $ do
            Env.addReqNode ref
            mapM_ markSuccessors =<< succ ref

-- interpreter

evaluateNode :: InterpreterCtx(m, ls, term) => Ref Node (ls :<: term) -> m ()
evaluateNode ref = do
    return ()

evaluateNodes :: InterpreterCtx(m, ls, term) => [Ref Node (ls :<: term)] -> m ()
evaluateNodes = mapM_ evaluateNode


#define PassCtx(m, ls, term) ( ls   ~ NetLayers a                                       \
                             , term ~ Draft Static                                      \
                             , ne   ~ Link (ls :<: term)                                \
                             , BiCastable e ne                                          \
                             , BiCastable n (ls :<: term)                               \
                             , MonadIO (m)                                              \
                             , MonadBuilder ((Hetero (VectorGraph n e c))) (m)          \
                             , NodeInferable (m) (ls :<: term)                          \
                             , TermNode Lam  (m) (ls :<: term)                          \
                             , MonadFix (m)                                             \
                             , HasProp InterpreterData (ls :<: term)                    \
                             , Prop    InterpreterData (ls :<: term) ~ InterpreterLayer \
                             )

run :: forall env m ls term ne a n e c. (PassCtx(InterpreterT env m, ls, term), MonadFix m, env ~ Env (Ref Node (ls :<: term)))
    => [Ref Node (ls :<: term)] -> m ()
run refsToEval = do
    -- ((), env) <- flip runInterpreterT (def :: env) $ markSuccessors ref
    ((), env) <- flip runInterpreterT (def :: env) $ evaluateNodes refsToEval
    return ()
