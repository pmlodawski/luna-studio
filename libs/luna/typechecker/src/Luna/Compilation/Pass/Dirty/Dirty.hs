{-# LANGUAGE CPP                       #-}

module Luna.Compilation.Pass.Dirty.Dirty where

import           Control.Monad                          (forM_)
import           Control.Monad.Trans.State
import qualified Data.IntSet                            as IntSet
import           Data.Prop
import           Development.Placeholders
import           Prologue                               hiding (pre, read, succ, ( # ), Getter, Setter)

import           Luna.Compilation.Pass.Dirty.Data.Env   (Env)
import qualified Luna.Compilation.Pass.Dirty.Data.Env   as Env
import           Luna.Compilation.Pass.Dirty.Data.Label (Dirty (Dirty), DirtyVal (DirtyVal))
import qualified Luna.Compilation.Pass.Dirty.Data.Label as Label
import           Luna.Compilation.Pass.Dirty.Monad      (DirtyMonad)
import           Luna.Syntax.Builder
import qualified Luna.Syntax.Model.Graph                as G
import           Luna.Syntax.Model.Graph.Builder.Class  (MonadBuilder)


import           Luna.Evaluation.Runtime                         (Dynamic, Static)

import           Data.Construction
import           Data.Record                                     hiding (cons)
import           Type.Inference

import           Luna.Syntax.Model.Network.Term
import           Luna.Syntax.Model.Graph
import           Luna.Syntax.Model.Graph.Builder
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Node.Inferred


#define PassCtx(m, ls, term, n) ( Castable n n                         \
                                , Castable e (Edge n n)                \
                                , Castable e (Edge n n)                \
                                , DirtyMonad (Env (Ref (Node n))) m    \
                                , Getter  Inputs n                     \
                                , HasProp Dirty n                      \
                                , HasProp Succs n                      \
                                , MonadBuilder  n e m                  \
                                , Prop Dirty    n ~ DirtyVal           \
                                , Prop Succs    n ~ [Ref (Edge n n)]   \
                                , Prop Inputs   n ~ [Ref (Edge n n)]   \
                                )

pre :: PassCtx(m, ls, term, n) => Ref (Node n) -> m [Ref (Node n)]
pre ref = do
    node <- read ref
    mapM (follow target) $ node # Inputs


succ :: PassCtx(m, ls, term, n) => Ref (Node n) -> m [Ref (Node n)]
succ ref = do
    node <- read ref
    mapM (follow source) $ node ^. prop Succs


isDirty :: (Prop Dirty n ~ DirtyVal, HasProp Dirty n) => n -> Bool
isDirty node = node ^. prop Dirty . Label.dirty


isRequired :: (Prop Dirty n ~ DirtyVal, HasProp Dirty n) => n -> Bool
isRequired node = node ^. prop Dirty . Label.required


followDirty :: PassCtx(m, ls, term, n) => Ref (Node n) -> m ()
followDirty ref = do
    Env.addReqNode ref
    prevs <- pre ref
    forM_ prevs $ \ p ->
        whenM (isDirty <$> read p) $
            followDirty p


markSuccessors :: PassCtx(m, ls, term, n) => Ref (Node n) -> m ()
markSuccessors ref = do
    node <- read ref
    unless (isDirty node) $ do
        write ref (node & prop Dirty . Label.dirty .~ True)
        when (isRequired node) $ do
            Env.addReqNode ref
            mapM_ markSuccessors =<< succ ref
