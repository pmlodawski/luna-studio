{-# LANGUAGE CPP                       #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}


module Luna.Syntax.Model.Graph where

import Prologue hiding (Ixed, Repr, repr)

import Control.Monad.Fix
import Data.Cata
import Data.Construction
import Data.Container
import Data.Container.Auto
import Data.Container.Class
import Data.Container.Hetero
import Data.Container.Opts       (ParamsOf, ModsOf)
import Data.Container.Poly
import Data.Container.Resizable
import Data.IntSet               (IntSet)
import Data.Reprx
import Data.Vector               hiding (convert, modify)
import Luna.Syntax.AST.Decl
import Luna.Syntax.AST.Term
import Luna.Syntax.Model.Layer.Labeled

import           Control.Monad.State.Dependent (StateT, MonadGet, MonadPut)
import qualified Control.Monad.State.Dependent as State

--import qualified Control.Monad.State as State
import qualified Data.IntSet as IntSet



-------------------------
-- === AutoVectors === --
-------------------------

#define AUTOVECTOR(a) (Auto Exponential (Vector a))
newtype AutoVector a = AutoVector AUTOVECTOR(a) deriving (Show, Default)
type instance Unlayered (AutoVector a) = AUTOVECTOR(a)

-- === Instances === --

-- Containers

type instance Container (AutoVector a) = Container (Unwrapped (AutoVector a))
instance Monad m => HasContainerM m (AutoVector a) where 
    viewContainerM = viewContainerM . unwrap
    setContainerM  = wrapped . setContainerM

instance Monad m => IsContainerM  m (AutoVector a) where
    fromContainerM = fmap AutoVector . fromContainerM

-- Wrappers

instance Layered   (AutoVector a)
instance Rewrapped (AutoVector a) (AutoVector a')
instance Wrapped   (AutoVector a) where
    type Unwrapped (AutoVector a) = Unlayered (AutoVector a)
    _Wrapped' = iso (\(AutoVector a) -> a) AutoVector



-------------------
-- === Graph === --
-------------------

data Graph node edge = Graph { _nodes :: AutoVector node
                             , _edges :: AutoVector edge
                             } deriving (Show)

makeLenses ''Graph
instance Default (Graph n e) where def = Graph (alloc 100) (alloc 100)



--------------------------
-- === GraphBuilder === --
--------------------------

data GB = GB deriving (Show)

newtype GraphBuilderT s m a = GraphBuilderT (StateT GB s m a)
    deriving (Functor, Monad, Applicative, MonadIO, MonadPlus, Alternative, MonadGet GB s, MonadPut GB s, MonadTrans, MonadFix)

--deriving instance Monad m => MonadGet GB s (GraphBuilderT s m)
--data GraphBuilder = GraphBuilder deriving (Show)


--newtype HRef a = HRef Int deriving (Show)
--newtype HRef2 (t :: * -> *) a = HRef2 Int deriving (Show)


--newtype Ref a = Ref a deriving (Show, Ord, Eq, Monoid, Functor, Foldable, Traversable)
--newtype Node' a = Node' a deriving (Show, Monoid, Functor, Foldable, Traversable)
--newtype Edge' a = Edge' a deriving (Show, Monoid, Functor, Foldable, Traversable)

--instance Rewrapped (Node' a) (Node' a')
--instance Wrapped   (Node' a) where
--    type Unwrapped (Node' a) = a
--    _Wrapped' = iso (\(Node' a) -> a) Node'

--instance Rewrapped (Edge' a) (Edge' a')
--instance Wrapped   (Edge' a) where
--    type Unwrapped (Edge' a) = a
--    _Wrapped' = iso (\(Edge' a) -> a) Edge'


--instance Rewrapped (Ref a) (Ref a')
--instance Wrapped   (Ref a) where
--    type Unwrapped (Ref a) = a
--    _Wrapped' = iso (\(Ref a) -> a) Ref

--type family   Derefd a
--type instance Derefd (Ref a) = Derefd a

--class               Deref a       where derefd :: Lens' a (Derefd a)
--instance Deref a => Deref (Ref a) where derefd = wrapped . derefd

--deref = view derefd


--newtype Node = Node Int deriving (Show, Ord, Eq)
--newtype Edge = Edge Int deriving (Show, Ord, Eq)

--instance Rewrapped Node Node
--instance Wrapped   Node where
--    type Unwrapped Node = Int
--    _Wrapped' = iso (\(Node a) -> a) Node

--instance Rewrapped Edge Edge
--instance Wrapped   Edge where
--    type Unwrapped Edge = Int
--    _Wrapped' = iso (\(Edge a) -> a) Edge


--type instance Derefd Node = Int
--type instance Derefd Edge = Int

--instance Deref Node where derefd = wrapped
--instance Deref Edge where derefd = wrapped


--data DoubleArc = DoubleArc { _source :: Ref Node, _target :: Ref Node } deriving (Show)



--data SuccTracking a = SuccTracking IntSet a deriving (Show)
--type instance Unlayered (SuccTracking a) = a
--instance      Layered   (SuccTracking a) where layered = lens (\(SuccTracking _ a) -> a) (\(SuccTracking i _) a -> SuccTracking i a)

--class TracksSuccs a where succs :: Lens' a IntSet
--instance {-# OVERLAPPABLE #-}                                           TracksSuccs (SuccTracking a) where succs = lens (\(SuccTracking ixs _) -> ixs) (\(SuccTracking _ a) ixs -> SuccTracking ixs a)
--instance {-# OVERLAPPABLE #-} (TracksSuccs (Unlayered a), Layered a) => TracksSuccs a                where succs = layered . succs


--type instance Destructed (SuccTracking a) = a

---- FIXME [WD] - usunac wszystkie poprzednie nody - wywolujac wunkcje do usuwania powiazania wsteczniego
--instance Monad m => Destructor m (SuccTracking a) where destruct (SuccTracking s a) = return a









--type instance Container (Graph n e) = Graph n e






--type instance ParamsOf AddableOp (Graph n e) = ParamsOf AddableOp (Container (AutoVector n))
--type instance ModsOf   AddableOp (Graph n e) = ModsOf   AddableOp (Container (AutoVector n))



--data Graph2 = Graph2 { _nodes2 :: AutoVector Int
--                     , _edges2 :: AutoVector Int
--                     } deriving (Show)

--instance Default Graph2 where def = Graph2 (alloc 100) (alloc 100)




--type instance DataStoreOf (AutoVector a) = DataStoreOf (Auto (Weak Vector) a)

--instance IsContainer  (AutoVector a) where fromContainer = AutoVector . fromContainer
--instance HasContainer (AutoVector a) where container     = lens (\(AutoVector a) -> a) (const AutoVector) . container

--makeLenses ''HeteroAutoVector

--instance HasContainer HeteroAutoVector   (Hetero' Vector) where container = _hetReg


---- === Ref ===

--newtype WeakMu a t   = WeakMu (Weak (a t))
--type HomoGraph ref t = AutoVector (t (Mu (ref t)))
--type ArcPtr          = Ref Int
--type Arc           a = Mu (ArcPtr a)

--newtype Ref i a t = Ref { fromRef :: Ptr i (a t) } deriving (Show)

----newtype DoubleArc a t = DoubleArc {__source :: ,}

--instance Repr s i => Repr s (Ref i a t) where repr = repr . fromRef



-- Instances





        --instance Content (Ref i a t) (Ref i' a' t') (Ptr i (a t)) (Ptr i' (a' t')) where
        --    content = lens fromRef (const Ref)

------------------------------------------


--data g = BldrState { _orphans :: [Int]
--                             , _graph   :: g
--                             }

--makeLenses ''BldrState

--class Hasg m | m -> g where
--    bldrState :: Lens' m g





--class Appendable     q m cont     el cont' | q m cont     el -> cont' where append    :: InstModsX Appendable      q m cont ->        el -> cont -> cont'

--instance Default g => Default g where
--    def = BldrState def def

        --instance (t ~ Ref i a, MonadIO m, Ixed (AddableM (a (Mu t)) (ASTBuilderT g m)) g, PtrFrom idx i, idx ~ IndexOf' (DataStoreOf (Container g)))
        --      => MuBuilder a (ASTBuilderT g m) t where
        --    buildMu a = do print ("oh" :: String)
        --                   fmap (Mu . Ref . ptrFrom) . modifyM2 $ ixed addM a


--instance (t ~ Ref i a, Monad m, Ixed (AddableM (ASTBuilderT g m)) (a (Mu t)) g, PtrFrom (IndexOf (Container g)) i)
--      => MuBuilder a (ASTBuilderT g m) t where
--    buildMu a = fmap (Mu . Ref . ptrFrom) . modifyM $ ixed addM a



--instance (t ~ Ref i a, MonadIO m, Ixed (Addable (WeakMu a (Mu t))) g, PtrFrom idx i, idx ~ IndexOf' (DataStoreOf (Container g)))
--      => MuBuilder a (ASTBuilderT g m) t where
--    buildMu a = do print ("oh" :: String)
--                   wptr <- liftIO $ mkWeakPtr a Nothing
--                   fmap (Mu . Ref . ptrFrom) . withGraph' $ ixed add (WeakMu wptr)



--class    Monad m => MuBuilder a m             t | t m -> a where buildMu :: a (Mu t) -> m (Mu t)
--instance Monad m => MuBuilder a (IdentityT m) a            where buildMu = return . Mu
--instance            MuBuilder a Identity      a            where buildMu = return . Mu


--mkWeakPtr :: k -> Maybe (IO ()) -> IO (Weak k)

--instance (t ~ Ref i a, Monad m, Appendable' cont idx (a (Mu t)), HasContainer g cont, PtrFrom idx i)
--      => MuBuilder a (ASTBuilderT g m) t where
--    buildMu a = fmap (Mu . Ref . ptrFrom) . withGraph . append' $ a

--foo = ixed append

--switch' (a,b) = (b,a)
