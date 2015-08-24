{-# LANGUAGE NoMonomorphismRestriction #-}
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

newtype HeteroVectorGraph   = HeteroVectorGraph { __hetReg :: Hetero' Vector } deriving (Show, Default)
newtype VectorGraph       a = VectorGraph       { __homReg :: Vector a       } deriving (Show, Default)

makeLenses ''HeteroVectorGraph
makeLenses ''VectorGraph

instance HasContainer HeteroVectorGraph   (Hetero' Vector) where container = _hetReg
instance HasContainer (VectorGraph a) (Vector a)       where container = _homReg


---- === Ref ===

type HomoGraph ref t = VectorGraph (t (Mu (ref t)))
type ArcPtr          = Ref Int
type Arc           a = Mu (ArcPtr a)

newtype Ref i a t = Ref { fromRef :: Ptr i (a t) } deriving (Show)

class     Monad m           => ToMRef a          m t | a -> t where toMRef :: a -> m (Mu t)
instance  Monad m           => ToMRef    (Mu t)  m t          where toMRef = return
instance (Monad m, (m ~ n)) => ToMRef (n (Mu t)) m t          where toMRef = id

-- === MuBuilder ===

type MuData' a = a (Mu a)

