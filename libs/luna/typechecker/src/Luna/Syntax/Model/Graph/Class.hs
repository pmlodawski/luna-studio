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

data Graph node edge = Graph { _nodes :: AutoVector node
                             , _edges :: AutoVector edge
                             } deriving (Show)
makeLenses ''Graph

data ELEMENT    = ELEMENT    deriving (Show)
data CONNECTION = CONNECTION deriving (Show)


-- === Instances === --

instance Default (Graph n e) where def = Graph (alloc 100) (alloc 100)









