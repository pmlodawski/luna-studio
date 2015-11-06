{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Luna.Syntax.Builder.Graph where

import Prologue               hiding (Ixed, Repr, repr)
import Data.Vector            hiding (convert, modify)
import Data.Container
import Data.Container.Hetero
import Data.Cata
import Control.Monad.Fix
import Data.Container.Class
import Data.Container.Poly
import Data.Container.Auto
import Data.Container.Weak
import Data.Container.Resizable
import Data.Reprx

import qualified Control.Monad.State as State
--import System.Mem.Weak

import Luna.Syntax.AST.Decl

--- === Graph ===

#define GRAPH (Auto' Exponential (Vector a))
--newtype HeteroVectorGraph   = HeteroVectorGraph { __hetReg :: Hetero' Vector } deriving (Show, Default)
newtype VectorGraph       a = VectorGraph       { __homReg :: GRAPH } deriving (Show, Default) -- , Functor, Foldable, Traversable)
--newtype VectorGraph       a = VectorGraph       { __homReg :: Vector a       } deriving (Show, Default) -- , Functor, Foldable, Traversable)
--newtype VectorGraph       a = VectorGraph       { __homReg :: Auto (Weak Vector) a       } deriving (Show, Default, Functor, Foldable, Traversable)

--type instance DataStoreOf (VectorGraph a) = DataStoreOf (Auto (Weak Vector) a)
type instance ContainerOf (VectorGraph a) = ContainerOf GRAPH

--instance IsContainer  (VectorGraph a) where fromContainer = VectorGraph . fromContainer
--instance HasContainer (VectorGraph a) where container     = lens (\(VectorGraph a) -> a) (const VectorGraph) . container

--makeLenses ''HeteroVectorGraph
makeLenses ''VectorGraph

--instance HasContainer HeteroVectorGraph   (Hetero' Vector) where container = _hetReg
instance Monad m => HasContainerM m (VectorGraph a)      where viewContainerM = viewContainerM .  view _homReg
                                                               setContainerM = _homReg . setContainerM


---- === Ref ===

--newtype WeakMu a t   = WeakMu (Weak (a t))
type HomoGraph ref t = VectorGraph (t (Mu (ref t)))
type ArcPtr          = Ref Int
type Arc           a = Mu (ArcPtr a)

newtype Ref i a t = Ref { fromRef :: Ptr i (a t) } deriving (Show)

instance Repr s i => Repr s (Ref i a t) where repr = repr . fromRef


        --instance Content (Ref i a t) (Ref i' a' t') (Ptr i (a t)) (Ptr i' (a' t')) where
        --    content = lens fromRef (const Ref)

------------------------------------------


--data g = BldrState { _orphans :: [Int]
--                             , _graph   :: g
--                             }

--makeLenses ''BldrState

--class Hasg m | m -> g where
--    bldrState :: Lens' m g


-- TODO: template haskellize
-- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

newtype GraphBuilderT g m a = GraphBuilderT { fromGraphBuilderT :: State.StateT g m a }
                             deriving (Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans, Alternative, MonadFix)

type GraphBuilder g = GraphBuilderT g Identity

class Monad m => MonadGraphBuilder g m | m -> g where
    get :: m g
    put :: g -> m ()

instance Monad m => MonadGraphBuilder g (GraphBuilderT g m) where
    get = GraphBuilderT State.get
    put = GraphBuilderT . State.put

instance State.MonadState s m => State.MonadState s (GraphBuilderT g m) where
    get = GraphBuilderT (lift State.get)
    put = GraphBuilderT . lift . State.put

instance {-# OVERLAPPABLE #-} (MonadGraphBuilder g m, MonadTrans t, Monad (t m)) => MonadGraphBuilder g (t m) where
    get = lift get
    put = lift . put

runT  ::            GraphBuilderT g m a -> g -> m (a, g)
evalT :: Monad m => GraphBuilderT g m a -> g -> m a
execT :: Monad m => GraphBuilderT g m a -> g -> m g

runT  = State.runStateT  . fromGraphBuilderT
evalT = State.evalStateT . fromGraphBuilderT
execT = State.execStateT . fromGraphBuilderT


run  :: GraphBuilder g a -> g -> (a, g)
eval :: GraphBuilder g a -> g -> a
exec :: GraphBuilder g a -> g -> g

run   = runIdentity .: runT
eval  = runIdentity .: evalT
exec  = runIdentity .: execT

with :: MonadGraphBuilder g m => (g -> g) -> m b -> m b
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out

modify :: MonadGraphBuilder g m => (g -> (g, a)) -> m a
modify = modifyM . fmap return

modifyM :: MonadGraphBuilder g m => (g -> m (g, a)) -> m a
modifyM f = do
    s <- get
    (s', a) <- f s
    put $ s'
    return a

modifyM2 :: MonadGraphBuilder g m => (g -> m (a, g)) -> m a
modifyM2 f = do
    s <- get
    (a, s') <- f s
    put $ s'
    return a

modify_ :: MonadGraphBuilder g m => (g -> g) -> m ()
modify_ = modify . fmap (,())

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<

withGraph :: MonadGraphBuilder g m => (g -> (g, a)) -> m a
withGraph = withGraphM . fmap return

withGraph' :: MonadGraphBuilder g m => (g -> (a, g)) -> m a
withGraph' = withGraphM . fmap (return . switch')

withGraph_ :: MonadGraphBuilder g m => (g -> g) -> m ()
withGraph_ = withGraph . fmap (,())

withGraphM :: MonadGraphBuilder g m => (g -> m (g, a)) -> m a
withGraphM = modifyM

withGraphM_ :: MonadGraphBuilder g m => (g -> m g) -> m ()
withGraphM_ = withGraphM . (fmap . fmap) (,())

runBuilderT  :: Functor m => GraphBuilderT g m a -> g -> m (a, g)
execBuilderT :: Monad   m => GraphBuilderT g m a -> g -> m g
evalBuilderT :: Monad   m => GraphBuilderT g m a -> g -> m a

runBuilderT  = runT
execBuilderT = execT
evalBuilderT = evalT


--class Appendable     q m cont     el cont' | q m cont     el -> cont' where append    :: InstModsX Appendable      q m cont ->        el -> cont -> cont'

--instance Default g => Default g where
--    def = BldrState def def

        --instance (t ~ Ref i a, MonadIO m, Ixed (AddableM (a (Mu t)) (GraphBuilderT g m)) g, PtrFrom idx i, idx ~ IndexOf' (DataStoreOf (ContainerOf g)))
        --      => MuBuilder a (GraphBuilderT g m) t where
        --    buildMu a = do print ("oh" :: String)
        --                   fmap (Mu . Ref . ptrFrom) . modifyM2 $ ixed addM a


instance (t ~ Ref i a, MonadIO m, Ixed (AddableM (GraphBuilderT g m)) (a (Mu t)) g, PtrFrom (IndexOf (ContainerOf g)) i)
      => MuBuilder a (GraphBuilderT g m) t where
    buildMu a = fmap (Mu . Ref . ptrFrom) . modifyM $ ixed addM a



--instance (t ~ Ref i a, MonadIO m, Ixed (Addable (WeakMu a (Mu t))) g, PtrFrom idx i, idx ~ IndexOf' (DataStoreOf (ContainerOf g)))
--      => MuBuilder a (GraphBuilderT g m) t where
--    buildMu a = do print ("oh" :: String)
--                   wptr <- liftIO $ mkWeakPtr a Nothing
--                   fmap (Mu . Ref . ptrFrom) . withGraph' $ ixed add (WeakMu wptr)



--class    Monad m => MuBuilder a m             t | t m -> a where buildMu :: a (Mu t) -> m (Mu t)
--instance Monad m => MuBuilder a (IdentityT m) a            where buildMu = return . Mu
--instance            MuBuilder a Identity      a            where buildMu = return . Mu


--mkWeakPtr :: k -> Maybe (IO ()) -> IO (Weak k)

--instance (t ~ Ref i a, Monad m, Appendable' cont idx (a (Mu t)), HasContainer g cont, PtrFrom idx i)
--      => MuBuilder a (GraphBuilderT g m) t where
--    buildMu a = fmap (Mu . Ref . ptrFrom) . withGraph . append' $ a

--foo = ixed append

switch' (a,b) = (b,a)
