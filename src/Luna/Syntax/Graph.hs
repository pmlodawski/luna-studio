{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Graph where

import Flowbox.Prelude
import Data.Vector            hiding (convert)
import Data.Containers
import Data.Containers.Hetero

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

---- === Ref ===

type Rec h a = h (a h)

newtype Ref     h a = Ref { fromRef :: Rec h a }
type    Wrapped m a = m (a (HPtr Int m))
type    Simple    a = a (Ptr Int)

-- utils

class Monad m => ToMRef t m l a | t -> l a where
    toMRef :: t -> m (GraphRef l a)

-- instances

deriving instance Show (h (a h)) => Show (Ref h a)

instance                             (Monad m) => ToMRef    (GraphRef l a)  m l a where toMRef = return
instance {-# OVERLAPPABLE #-} (m ~ n, Monad m) => ToMRef (m (GraphRef l a)) n l a where toMRef = id



-- === RefBuilder ===

class Monad m => RefBuilder el m ref | el m -> ref, ref m -> el where
    mkRef :: el -> m ref


