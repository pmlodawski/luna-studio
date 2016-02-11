{-# LANGUAGE CPP                       #-}

module Luna.Compilation.Pass.Dirty.Handler where

import           Data.Prop
import           Development.Placeholders
import           Prologue hiding (read, Getter, Setter)

import           Luna.Compilation.Pass.Dirty.Data.Env   (Env)
import qualified Luna.Compilation.Pass.Dirty.Data.Env   as Env
import qualified Luna.Compilation.Pass.Dirty.Data.Env   as Env
import           Luna.Compilation.Pass.Dirty.Data.Label (Dirty (Dirty), DirtyVal (DirtyVal))
import qualified Luna.Compilation.Pass.Dirty.Data.Label as Label
import qualified Luna.Compilation.Pass.Dirty.Dirty      as Dirty
import           Luna.Compilation.Pass.Dirty.Monad      (DirtyMonad)
import           Luna.Syntax.Builder
import qualified Luna.Syntax.Model.Graph                as G
import           Luna.Syntax.Model.Graph.Builder.Class  (MonadBuilder)


-- #define PassCtx(m, ls, term, n) ( Castable n n                         \
--                                 , Castable e (Edge n n)                \
--                                 , DirtyMonad (Env (Ref (Node n))) m    \
--                                 , Getter  Inputs n                     \
--                                 , HasProp Dirty n                      \
--                                 , HasProp Succs n                      \
--                                 , MonadBuilder  n e m                  \
--                                 , Prop Dirty    n ~ DirtyVal           \
--                                 , Prop Succs    n ~ [Ref (Edge n n)]   \
--                                 , Prop Inputs   n ~ [Ref (Edge n n)]   \
--                                 )


-- nodesToExecute :: PassCtx(m, ls, term, n) =>  m [Ref (Node n)]
-- nodesToExecute = do
--     mapM_ Dirty.followDirty =<< Env.getReqNodes
--     Env.getReqNodes


-- reset :: DirtyMonad (Env node) m => m ()
-- reset = Env.clearReqNodes


-- connect :: PassCtx(m, ls, term, n) => Ref (Node n) -> Ref (Node n) -> m ()
-- connect prev next = do
--     isPrevDirty <- Dirty.isDirty <$> read prev
--     Dirty.markSuccessors $ if isPrevDirty
--         then prev
--         else next


-- markModified :: PassCtx(m, ls, term, n) => Ref (Node n) -> m ()
-- markModified = Dirty.markSuccessors
