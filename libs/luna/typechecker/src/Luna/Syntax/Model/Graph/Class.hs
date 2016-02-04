{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Luna.Syntax.Model.Graph.Class where

import Prologue hiding (Getter, Setter, read)

import Control.Monad.Event
import Data.Attribute
import Data.Construction
import Data.Container
import Data.Container.Auto      (Auto)
import Data.Container.Resizable (Exponential)
import Data.Index
import Data.Vector              (Vector)

-------------------------
-- === AutoVectors === --
-------------------------

newtype AutoVector a = AutoVector (Auto Exponential (Vector a)) deriving (Show, Default)


-- === Instances === --

-- Containers
type instance Container (AutoVector a) = Container (Unwrapped (AutoVector a))
instance Monad m => HasContainerM m (AutoVector a) where 
    viewContainerM = viewContainerM . unwrap ; {-# INLINE viewContainerM #-}
    setContainerM  = wrapped . setContainerM ; {-# INLINE setContainerM  #-}

instance Monad m => IsContainerM  m (AutoVector a) where
    fromContainerM = fmap AutoVector . fromContainerM ; {-# INLINE fromContainerM #-}

-- Wrappers
makeWrapped ''AutoVector
type instance Unlayered (AutoVector a) = Unwrapped (AutoVector a)
instance      Layered   (AutoVector a)



-------------------
-- === Graph === --
-------------------

data Network (ls :: [*]) = Network
data Graph node edge = Graph { _nodes :: AutoVector node
                             , _edges :: AutoVector edge
                             } deriving (Show)
makeLenses ''Graph

data ELEMENT    = ELEMENT    deriving (Show)
data CONNECTION = CONNECTION deriving (Show)


-- === Instances === --

instance Default (Graph n e) where def = Graph (alloc 100) (alloc 100)



------------------------------
-- === Graph references === --
------------------------------

-- === Definitions === --

data Ptr i = Ptr i         deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
data Ref a = Ref (Ptr Int) deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

makeWrapped ''Ptr
makeWrapped ''Ref

type family Target a

class HasPtr   a where ptr   :: Lens' a (Ptr (Index  a))
class HasRef   a where ref   :: Lens' a (Ref (Target a))

class Monad m => Reader m a where read  :: Ref a -> m a
class Monad m => Writer m a where write :: Ref a -> a -> m ()

type Modifier m a = (Reader m a, Writer m a)


-- === Utils === --

rawPtr :: HasPtr a => Lens' a (Index a)
rawPtr = ptr ∘ wrapped'

withM :: Modifier m a => Ref a -> (a -> m a) -> m ()
withM ref f = read ref >>= f >>= write ref

with :: Modifier m a => Ref a -> (a -> a) -> m ()
with ref = withM ref ∘ (return <$>)


-- === Instances === --

-- Ptr primitive instances
type instance Index  (Ptr i) = i
instance      HasIdx (Ptr i) where idx = wrapped'
instance      HasPtr (Ptr i) where ptr = id

-- Ref primitive instances
type instance Unlayered  (Ref a) = a
type instance Destructed (Ref a) = a
type instance Target     (Ref a) = a
type instance Index      (Ref a) = Index (Unwrapped (Ref a))
instance      HasRef     (Ref a) where ref = id
instance      HasIdx     (Ref a) where idx = ptr ∘ idx
instance      HasPtr     (Ref a) where ptr = wrapped'

-- Conversions
instance Castable a a' => Castable (Ref a) (Ref a') where cast = rewrap ; {-# INLINE cast #-}

-- Construction
instance Constructor m (Ref ref) => LayerConstructor m (Ref ref) where
    constructLayer = construct ; {-# INLINE constructLayer #-}



--------------------------------
-- === Network Structures === --
--------------------------------

-- === Definitions === --

newtype Node       a = Node a                   deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
data    Edge src tgt = Edge (Ref src) (Ref tgt) deriving (Show, Eq, Ord)
type    Link       a = Edge a a

type family Connection  src dst
type family Conn_Source a -- TODO[WD]: change name after refactoring
type family Conn_Target a -- TODO[WD]: change name after refactoring

class ( src ~ Conn_Source (Connection src tgt)
      , tgt ~ Conn_Target (Connection src tgt)
      ) => Connectible src tgt m where connection :: src -> tgt -> m (Connection src tgt)

-- Properties
data Inputs  = Inputs  deriving (Show)
data Outputs = Outputs deriving (Show)


-- === Utils === --

edge :: Ref (Node src) -> Ref (Node tgt) -> Edge src tgt
edge src tgt = Edge (rewrap src) (rewrap tgt)

source :: Lens' (Edge src tgt) (Ref (Node src))
source = lens (\(Edge src _) -> rewrap src) (\(Edge _ tgt) src -> Edge (rewrap src) tgt)

target :: Lens' (Edge src tgt) (Ref (Node tgt))
target = lens (\(Edge _ tgt) -> rewrap tgt) (\(Edge src _) tgt -> Edge src (rewrap tgt))

follow :: (Reader m (Edge src tgt), Functor m) => Ref (Edge src tgt) -> m (Ref $ Node tgt)
follow ptr = view target <$> read ptr



-- === Instances === --

-- Primitive

type instance Conn_Source (Ref a) = Ref (Conn_Source a)
type instance Conn_Target (Ref a) = Ref (Conn_Target a)
type instance Conn_Source (Edge src tgt) = Node src
type instance Conn_Target (Edge src tgt) = Node tgt

-- Wrappers

makeWrapped ''Node
type instance Unlayered (Node a) = (Unwrapped (Node a))
instance      Layered   (Node a)

-- Connecting

type instance Connection (Ref  a) (Ref  b) = Ref (Connection a b)
type instance Connection (Node a) (Node b) = Edge a b

instance (LayerConstructor m c, Dispatcher CONNECTION c m, Unlayered c ~ Edge src tgt, c ~ Connection (Ref (Node src)) (Ref (Node tgt))) 
      => Connectible (Ref (Node src)) (Ref (Node tgt)) m where
         connection src tgt = dispatch CONNECTION =<< constructLayer (edge src tgt)

-- Construction

instance Monad m => LayerConstructor m (Node a) where
    constructLayer = return ∘ Node ; {-# INLINE constructLayer #-}

-- Conversions

instance (Castable (Ref src) (Ref src'), Castable (Ref tgt) (Ref tgt')) => Castable (Edge src tgt) (Edge src' tgt') where 
    cast (Edge src tgt) = Edge (cast src) (cast tgt) ; {-# INLINE cast #-}

instance Castable a a' => Castable (Node a) (Node a') where
    cast = wrapped %~ cast

-- Attributes

type instance            Attr a (Node t) = Attr a t
instance Getter a t => Getter a (Node t) where getter a = getter a ∘ unwrap'      ; {-# INLINE getter #-}
instance Setter a t => Setter a (Node t) where setter   = over wrapped' ∘∘ setter ; {-# INLINE setter #-}
