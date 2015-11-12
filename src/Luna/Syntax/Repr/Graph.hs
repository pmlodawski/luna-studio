{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Luna.Syntax.Repr.Graph where

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

import Luna.Syntax.Layer.Labeled
import Luna.Syntax.AST.Typed
import Luna.Syntax.AST.Term

import qualified Control.Monad.State as State
--import System.Mem.Weak

import Luna.Syntax.AST.Decl
import Data.Layer.Coat

--- === Graph ===

#define VECTORGRAPH (Auto' Exponential (Vector a))
newtype VectorGraph a = VectorGraph { __homReg :: VECTORGRAPH } deriving (Show, Default)

instance Rewrapped (VectorGraph a) (VectorGraph a')
instance Wrapped   (VectorGraph a) where
	type Unwrapped (VectorGraph a) = VECTORGRAPH
	_Wrapped' = iso (\(VectorGraph a) -> a) VectorGraph

type instance ContainerOf (VectorGraph a) = ContainerOf VECTORGRAPH
instance Monad m => HasContainerM m (VectorGraph a)      where viewContainerM = viewContainerM . unwrap
                                                               setContainerM = wrapped . setContainerM











data DoubleArc = DoubleArc { _source :: Int, _target :: Int } deriving (Show)



data SuccTracking a = SuccTracking [Int] a deriving (Show)
type instance Unlayered (SuccTracking a) = a
instance      Layered   (SuccTracking a) where layered = lens (\(SuccTracking _ a) -> a) (\(SuccTracking i _) a -> SuccTracking i a) 

class TracksSuccs a where succs :: Lens' a [Int]
instance {-# OVERLAPPABLE #-}                                           TracksSuccs (SuccTracking a) where succs = lens (\(SuccTracking ixs _) -> ixs) (\(SuccTracking _ a) ixs -> SuccTracking ixs a)
instance {-# OVERLAPPABLE #-} (TracksSuccs (Unlayered a), Layered a) => TracksSuccs a                where succs = layered . succs



data Graph a = Graph { _nodes :: VectorGraph a
                     , _edges :: VectorGraph DoubleArc 
                     } deriving (Show)

makeLenses ''DoubleArc
makeLenses ''Graph

instance Default (Graph a) where def = Graph def def
















--type instance DataStoreOf (VectorGraph a) = DataStoreOf (Auto (Weak Vector) a)

--instance IsContainer  (VectorGraph a) where fromContainer = VectorGraph . fromContainer
--instance HasContainer (VectorGraph a) where container     = lens (\(VectorGraph a) -> a) (const VectorGraph) . container

--makeLenses ''HeteroVectorGraph

--instance HasContainer HeteroVectorGraph   (Hetero' Vector) where container = _hetReg


---- === Ref ===

--newtype WeakMu a t   = WeakMu (Weak (a t))
type HomoGraph ref t = VectorGraph (t (Mu (ref t)))
type ArcPtr          = Ref Int
type Arc           a = Mu (ArcPtr a)

newtype Ref i a t = Ref { fromRef :: Ptr i (a t) } deriving (Show)

--newtype DoubleArc a t = DoubleArc {__source :: ,}

instance Repr s i => Repr s (Ref i a t) where repr = repr . fromRef



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

        --instance (t ~ Ref i a, MonadIO m, Ixed (AddableM (a (Mu t)) (ASTBuilderT g m)) g, PtrFrom idx i, idx ~ IndexOf' (DataStoreOf (ContainerOf g)))
        --      => MuBuilder a (ASTBuilderT g m) t where
        --    buildMu a = do print ("oh" :: String)
        --                   fmap (Mu . Ref . ptrFrom) . modifyM2 $ ixed addM a


--instance (t ~ Ref i a, Monad m, Ixed (AddableM (ASTBuilderT g m)) (a (Mu t)) g, PtrFrom (IndexOf (ContainerOf g)) i)
--      => MuBuilder a (ASTBuilderT g m) t where
--    buildMu a = fmap (Mu . Ref . ptrFrom) . modifyM $ ixed addM a



--instance (t ~ Ref i a, MonadIO m, Ixed (Addable (WeakMu a (Mu t))) g, PtrFrom idx i, idx ~ IndexOf' (DataStoreOf (ContainerOf g)))
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
