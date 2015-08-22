{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Luna.Syntax.Graph where

import Flowbox.Prelude
import Data.Vector            hiding (convert)
import Data.Containers
import Data.Containers.Hetero
import Data.Cata

import Luna.Syntax.Decl

--- === Graph ===

newtype HeteroGraph   = HeteroGraph { __hetReg :: Hetero' Vector } deriving (Show, Default)
newtype HomoGraph   a = HomoGraph   { __homReg :: Vector a       } deriving (Show, Default)

makeLenses ''HeteroGraph
makeLenses ''HomoGraph

instance HasContainer HeteroGraph   (Hetero' Vector) where container = _hetReg
instance HasContainer (HomoGraph a) (Vector a)       where container = _homReg


newtype GraphRef l a = GraphRef { fromGraphRef :: Ref (GraphPtr l) a } deriving (Show)

type GraphPtr  l   = HPtr Int l
type GraphNode l a = a (GraphPtr l)
type HomoNet   l a = HomoGraph (l (GraphNode l a))
type HeteroNet     = HeteroGraph

type MuRefBuilder m ref a = RefBuilder3 m ref (MuRefData ref a)


---- === Ref ===

type Recx h a = h (a h)

newtype Ref     h a = Ref { fromRef :: Recx h a }
type    Wrapped m a = m (a (HPtr Int m))
type    Simple    a = a (Ptr Int)

data Ref'      ref a t =     Ref' (ref (a t)) deriving (Show)
type MuRef     ref a   = Mu (Ref' ref a)
type MuRefData ref a   = a (MuRef ref a)

-- utils

class Monad m => ToMRef t m l a | t -> l a where
    toMRef :: t -> m (GraphRef l a)

-- instances

deriving instance Show (h (a h)) => Show (Ref h a)

instance Repr (ref (a t)) => Repr (Ref' ref a t) where repr (Ref' r) = "Ref (" <> repr r <> ")"

instance                             (Monad m) => ToMRef    (GraphRef l a)  m l a where toMRef = return
instance {-# OVERLAPPABLE #-} (m ~ n, Monad m) => ToMRef (m (GraphRef l a)) n l a where toMRef = id

instance (Typeable a, Typeable l) => Repr (GraphRef l a) where
    repr = repr . fromGraphRef

instance Repr (Recx h a) => Repr (Ref h a) where
    repr = repr . fromRef

-- === RefBuilder ===

--class Monad m => RefBuilder el m ref | el m -> ref, ref m -> el where
--    mkRef :: el -> m ref


--class Monad m => RefBuilder2 a t m ptr | m -> ptr where
--    mkRef2 :: a t -> m (ptr a t)


class Monad m => RefBuilder3 m ref a | a m -> ref where -- FIXME? [WD]: usunac a z fundepow?
    mkRef3 :: a -> m (ref a)


mkMuRef :: MuRefBuilder m ref a => a (MuRef ref a) -> m (MuRef ref a)
mkMuRef = fmap (Mu . Ref') . mkRef3


