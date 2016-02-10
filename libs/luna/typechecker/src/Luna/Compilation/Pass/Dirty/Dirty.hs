module Luna.Compilation.Pass.Dirty.Dirty where

import           Control.Monad                          (forM_)
import           Control.Monad.Trans.State
import qualified Data.IntSet                            as IntSet
import           Data.Prop
import           Development.Placeholders
import           Prologue                               hiding (pre, read, succ, ( # ))

import           Luna.Compilation.Pass.Dirty.Data.Env   (Env)
import qualified Luna.Compilation.Pass.Dirty.Data.Env   as Env
import           Luna.Compilation.Pass.Dirty.Data.Label (Dirty (Dirty), DirtyVal (DirtyVal))
import qualified Luna.Compilation.Pass.Dirty.Data.Label as Label
import           Luna.Syntax.Builder
import qualified Luna.Syntax.Model.Graph                as G
import           Luna.Syntax.Model.Graph.Builder.Class  (MonadBuilder)



pre :: ( Castable n n
       , Castable e (Edge src n)
       , Data.Prop.Getter Inputs n
       , MonadBuilder n e m
       , Prop Inputs n ~ [Ref (Edge src n)])
    => Ref (Node n) -> m [Ref (Node n)]
pre ref = do
    node <- read ref
    mapM (follow target) $ node # Inputs


succ :: ( Castable n n
        , Castable e (Edge n tgt)
        , HasProp Succs n
        , MonadBuilder n e m
        , Prop Succs n ~ [Ref (Edge n tgt)])
     => Ref (Node n) -> m [Ref $ Node n]
succ ref = do
    node <- read ref
    mapM (follow source) $ node ^. prop Succs


isDirty :: (Prop Dirty n ~ DirtyVal, HasProp Dirty n) => n -> Bool
isDirty node = node ^. prop Dirty . Label.dirty


isRequired :: (Prop Dirty n ~ DirtyVal, HasProp Dirty n) => n -> Bool
isRequired node = node ^. prop Dirty . Label.required

followDirty' :: ( Castable n n
                , Castable e (Edge src n)
                , Data.Prop.Getter Inputs n
                , HasProp Dirty n
                , MonadBuilder n e m
                , Prop Inputs n ~ [Ref (Edge src n)]
                , Prop Dirty n ~ DirtyVal)
             => Ref (Node n) -> StateT (Env (Ref (Node n))) m ()
followDirty' node = do
    Env.addReqNode node
    prevs <- pre node
    forM_ prevs $ \ p ->
        whenM (isDirty <$> read p) $
            followDirty' p


markSuccessors :: ( Castable n n
                  , Castable e (Edge n tgt)
                  , HasProp Dirty n
                  , HasProp Succs n
                  , MonadBuilder n e m
                  , Prop Dirty n ~ DirtyVal
                  , Prop Succs n ~ [Ref (Edge n tgt)])
               => Ref (Node n) -> m [Ref (Node n)]
markSuccessors ref = view Env.reqNodes <$> (flip execStateT def $ markSuccessors' ref)


markSuccessors' :: ( Castable n n
                   , Castable e (Edge n tgt)
                   , HasProp Dirty n
                   , HasProp Succs n
                   , MonadBuilder n e m
                   , Prop Dirty n ~ DirtyVal
                   , Prop Succs n ~ [Ref (Edge n tgt)])
               => Ref (Node n) -> StateT (Env (Ref (Node n))) m ()
markSuccessors' ref = do
    node <- read ref
    unless (isDirty node) $ do
        write ref (node & prop Dirty . Label.dirty .~ True)
        when (isRequired node) $ do
            Env.addReqNode ref
            mapM_ markSuccessors' =<< succ ref
