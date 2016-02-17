{-# LANGUAGE CPP                       #-}

module Luna.Compilation.Pass.Dirty.Dirty where

import           Data.Graph
import           Control.Monad                                   (forM_)
import           Control.Monad.Trans.State
import qualified Data.IntSet                                     as IntSet
import           Data.Prop
import           Development.Placeholders
import           Prologue                                        hiding (Getter, Setter, pre, read, succ, (#))

import           Luna.Compilation.Pass.Dirty.Data.Env            (Env)
import qualified Luna.Compilation.Pass.Dirty.Data.Env            as Env
import           Luna.Compilation.Pass.Dirty.Data.Label          (Dirty(..), Required(..))
import qualified Luna.Compilation.Pass.Dirty.Data.Label          as Label
import           Luna.Compilation.Pass.Dirty.Monad               (DirtyMonad, runDirtyT, DirtyT)
import           Luna.Syntax.Builder

import           Luna.Evaluation.Runtime                         (Dynamic, Static)

import           Data.Construction
import           Data.Record                                     hiding (cons)
import           Type.Inference

import           Luna.Syntax.AST.Term.Class                      (Lam)
import           Data.Graph.Builder
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Luna.Syntax.Model.Network.Term

import           Control.Monad.Event                             (Dispatcher)
import           Control.Monad.Trans.Identity
import           Data.Graph.Backend.VectorGraph


#define PassCtxDirty(m, ls, term) ( ls   ~ NetLayers a                             \
                                  , term ~ Draft Static                            \
                                  , ne   ~ Link (ls :< term)                       \
                                  , BiCastable e ne                                \
                                  , BiCastable n (ls :< term)                      \
                                  , MonadIO m                                      \
                                  , MonadBuilder (Hetero (VectorGraph n e c)) m    \
                                  , NodeInferable m (ls :< term)                   \
                                  , TermNode Lam  m (ls :< term)                   \
                                  , HasProp Dirty    (ls :< term)                  \
                                  , HasProp Required (ls :< term)                  \
                                  , Prop Dirty       (ls :< term) ~ Bool           \
                                  , Prop Required    (ls :< term) ~ Bool           \
                                  , DirtyMonad (Env (Ref Node (ls :< term))) m     \
                                  )



pre :: PassCtxDirty(m, ls, term) => Ref Node (ls :< term) -> m [Ref Node (ls :< term)]
pre ref = do
    node <- read ref
    mapM (follow target) $ node # Inputs

succ :: PassCtxDirty(m, ls, term) => Ref Node (ls :< term) -> m [Ref Node (ls :< term)]
succ ref = do
    node <- read ref
    mapM (follow source) $ node ^. prop Succs

-- isDirty :: (Prop Dirty n ~ Bool, HasProp Dirty n) => n -> Bool
-- isDirty node = node ^. prop Dirty

-- isRequired :: (Prop Required n ~ Bool, HasProp Required n) => n -> Bool
-- isRequired node = node ^. prop Required

followDirty :: PassCtxDirty(m, ls, term) => Ref Node (ls :< term) -> m ()
followDirty ref = do
    Env.addReqNode ref
    prevs <- pre ref
    forM_ prevs $ \p -> do
        nd <- read p
        let dirty = nd # Dirty
        when (dirty) $
            followDirty p

markSuccessors :: PassCtxDirty(m, ls, term) => Ref Node (ls :< term) -> m ()
markSuccessors ref = do
    node <- read ref
    -- putStrLn $         "markSuccessors " <> show ref
    unless (node ^. prop Dirty) $ do
        -- putStrLn $     "marking dirty  " <> show ref
        write ref (node & prop Dirty .~ True)
        when (node # Required) $ do
            -- putStrLn $ "addReqNode     " <> show ref
            Env.addReqNode ref
            mapM_ markSuccessors =<< succ ref


#define PassCtx(m, ls, term) ( ls   ~ NetLayers a                              \
                             , term ~ Draft Static                             \
                             , ne   ~ Link (ls :< term)                        \
                             , BiCastable e ne                                 \
                             , BiCastable n (ls :< term)                       \
                             , MonadIO (m)                                     \
                             , MonadBuilder ((Hetero (VectorGraph n e c))) (m) \
                             , NodeInferable (m) (ls :< term)                  \
                             , TermNode Lam  (m) (ls :< term)                  \
                             , MonadFix (m)                                    \
                             , HasProp Dirty    (ls :< term)                   \
                             , HasProp Required (ls :< term)                   \
                             , Prop Dirty       (ls :< term) ~ Bool            \
                             , Prop Required    (ls :< term) ~ Bool            \
                             )

                             -- , HasProp Dirty (ls :< term)                      \
                             -- , HasProp Required (ls :< term)                   \

run :: forall env m ls term ne a n e c. (PassCtx(DirtyT env m, ls, term), MonadFix m, env ~ Env (Ref Node (ls :< term)))
    => Ref Node (ls :< term) -> m ()
run ref = do
    ((), env) <- flip runDirtyT (def :: env) $ markSuccessors ref
    return ()


-- runDirtyT  :: Functor m => DirtyT env m a -> env -> m (a, env)
-- ls :< Draft Static
