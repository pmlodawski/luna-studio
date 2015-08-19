{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Luna.Syntax.Graph.Builder.State where


import Flowbox.Prelude

import qualified Control.Monad.State as State
import           Data.Containers.Hetero
import           Luna.Syntax.Graph
import           Data.Variant
import           Data.Containers


data BldrState g = BldrState { _orphans :: [Int]
                             , _graph   :: g
                             }

type GraphRefBuilder  el m l a     = RefBuilder   el m (Ref (GraphPtr l) a)
type GraphConstructor base l a ast = Constructor (base (Rec (GraphPtr l) a)) ast

makeLenses ''BldrState

class HasBldrState g m | m -> g where
    bldrState :: Lens' m (BldrState g)


-- TODO: template haskellize
-- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

newtype GraphBuilderT g m a = GraphBuilderT { fromGraphBuilderT :: State.StateT (BldrState g) m a }
                              deriving (Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans, Alternative)

type GraphBuilder g = GraphBuilderT g Identity

class Monad m => MonadGraphBuilder g m | m -> g where
    get :: m (BldrState g)
    put :: BldrState g -> m ()

instance Monad m => MonadGraphBuilder g (GraphBuilderT g m) where
    get = GraphBuilderT State.get
    put = GraphBuilderT . State.put

instance State.MonadState s m => State.MonadState s (GraphBuilderT g m) where
    get = GraphBuilderT (lift State.get)
    put = GraphBuilderT . lift . State.put

instance {-# OVERLAPPABLE #-} (MonadGraphBuilder g m, MonadTrans t, Monad (t m)) => MonadGraphBuilder g (t m) where
    get = lift get
    put = lift . put

runT  ::            GraphBuilderT g m a -> BldrState g -> m (a, BldrState g)
evalT :: Monad m => GraphBuilderT g m a -> BldrState g -> m a
execT :: Monad m => GraphBuilderT g m a -> BldrState g -> m (BldrState g)

runT  = State.runStateT  . fromGraphBuilderT
evalT = State.evalStateT . fromGraphBuilderT
execT = State.execStateT . fromGraphBuilderT


run  :: GraphBuilder g a -> BldrState g -> (a, BldrState g)
eval :: GraphBuilder g a -> BldrState g -> a
exec :: GraphBuilder g a -> BldrState g -> (BldrState g)

run   = runIdentity .: runT
eval  = runIdentity .: evalT
exec  = runIdentity .: execT

with :: MonadGraphBuilder g m => (BldrState g -> BldrState g) -> m b -> m b
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out

modify :: MonadGraphBuilder g m => (BldrState g -> (BldrState g, a)) -> m a
modify f = do
    s <- get
    let (s', a) = f s
    put $ s'
    return a

modify_ :: MonadGraphBuilder g m => (BldrState g -> BldrState g) -> m ()
modify_ = modify . fmap (,())


-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<

withGraph :: MonadGraphBuilder g m => (g -> (g, a)) -> m a
withGraph = modify . mapOver graph

runGraphT :: Functor m => GraphBuilderT g m a -> BldrState g -> m (a, g)
runGraphT = fmap (over _2 $ view graph) .: runT


instance (Convertible idx (h (a h)), HasContainer g cont, Appendable cont idx el, Monad m, PtrTarget h a el)
      => RefBuilder el (GraphBuilderT g m) (Ref h a) where
    mkRef el = fmap (Ref . convert) . withGraph . append $ el

instance Default g => Default (BldrState g) where
    def = BldrState def def
