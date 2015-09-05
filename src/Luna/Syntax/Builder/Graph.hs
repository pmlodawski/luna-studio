{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Luna.Syntax.Builder.Graph where

import Prologue               hiding (Ixed)
import Data.Vector            hiding (convert, modify)
import Data.Containers
import Data.Containers.Hetero
import Data.Cata
import Control.Monad.Fix

import qualified Control.Monad.State as State

import Luna.Syntax.AST.Decl

--- === Graph ===

newtype HeteroVectorGraph   = HeteroVectorGraph { __hetReg :: Hetero' Vector } deriving (Show, Default)
newtype VectorGraph       a = VectorGraph       { __homReg :: Vector a       } deriving (Show, Default, Functor, Foldable, Traversable)

makeLenses ''HeteroVectorGraph
makeLenses ''VectorGraph

instance HasContainer HeteroVectorGraph   (Hetero' Vector) where container = _hetReg
instance HasContainer (VectorGraph a)     (Vector a)       where container = _homReg


---- === Ref ===

type HomoGraph ref t = VectorGraph (t (Mu (ref t)))
type ArcPtr          = Ref Int
type Arc           a = Mu (ArcPtr a)

newtype Ref i a t = Ref { fromRef :: Ptr i (a t) } deriving (Show)

instance Repr s i => Repr s (Ref i a t) where repr = repr . fromRef


instance Content (Ref i a t) (Ref i' a' t') (Ptr i (a t)) (Ptr i' (a' t')) where
    content = lens fromRef (const Ref)

------------------------------------------


data BldrState g = BldrState { _orphans :: [Int]
                             , _graph   :: g
                             }

makeLenses ''BldrState

class HasBldrState g m | m -> g where
    bldrState :: Lens' m (BldrState g)


-- TODO: template haskellize
-- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

newtype GraphBuilderT g m a = GraphBuilderT { fromGraphBuilderT :: State.StateT (BldrState g) m a }
                             deriving (Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans, Alternative, MonadFix)

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
modify = modifyM . fmap return

modifyM :: MonadGraphBuilder g m => (BldrState g -> m (BldrState g, a)) -> m a
modifyM f = do
    s <- get
    (s', a) <- f s
    put $ s'
    return a

modify_ :: MonadGraphBuilder g m => (BldrState g -> BldrState g) -> m ()
modify_ = modify . fmap (,())

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<

withGraph :: MonadGraphBuilder g m => (g -> (g, a)) -> m a
withGraph = withGraphM . fmap return

withGraph' :: MonadGraphBuilder g m => (g -> (a, g)) -> m a
withGraph' = withGraphM . fmap (return . switch')

withGraph_ :: MonadGraphBuilder g m => (g -> g) -> m ()
withGraph_ = withGraph . fmap (,())

withGraphM :: MonadGraphBuilder g m => (g -> m (g, a)) -> m a
withGraphM = modifyM . mapOverM graph

withGraphM_ :: MonadGraphBuilder g m => (g -> m g) -> m ()
withGraphM_ = withGraphM . (fmap . fmap) (,())

runBuilderT  :: Functor m => GraphBuilderT g m a -> BldrState g -> m (a, g)
execBuilderT :: Monad   m => GraphBuilderT g m a -> BldrState g -> m g
evalBuilderT :: Monad   m => GraphBuilderT g m a -> BldrState g -> m a

runBuilderT  = fmap (over _2 $ view graph) .: runT
execBuilderT = fmap (view graph) .: execT
evalBuilderT = evalT


--class Appendable     q m cont     el cont' | q m cont     el -> cont' where append    :: InstModsX Appendable      q m cont ->        el -> cont -> cont'

instance Default g => Default (BldrState g) where
    def = BldrState def def

--instance (t ~ Ref i a, Monad m, AppendableT '[Ixed] g (a (Mu t)) (idx, g), HasContainer g cont, PtrFrom idx i)
--      => MuBuilder a (GraphBuilderT g m) t where
--    buildMu a = fmap (Mu . Ref . ptrFrom) . withGraph' . ixed append $ a



--instance (t ~ Ref i a, Monad m, Appendable' cont idx (a (Mu t)), HasContainer g cont, PtrFrom idx i)
--      => MuBuilder a (GraphBuilderT g m) t where
--    buildMu a = fmap (Mu . Ref . ptrFrom) . withGraph . append' $ a

foo = ixed append

switch' (a,b) = (b,a)