
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}

import           Prelude       hiding (null)
import qualified Prelude       as P
import           Data.Default
import           Data.Monoid
import           Data.Maybe    (fromJust)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Foldable (toList)

class Nullable t where
    null :: t a -> Bool

instance Nullable []  where null = P.null
instance Nullable Seq where null = Seq.null

----------------------------------------------------------------------
-- General queue datatype
----------------------------------------------------------------------

newtype Queue t a = Queue { unQueue :: ContainerOf t a }

type family ContainerOf t :: (* -> *)

class Nullable (Queue t) => IsQueue t where
    push  :: a -> Queue t a -> Queue t a
    pop   :: Queue t a -> Maybe (a, Queue t a)

-- instances

deriving instance (Show (ContainerOf t a)) => Show (Queue t a)
deriving instance Functor (ContainerOf t)  => Functor (Queue t)
deriving instance Nullable (ContainerOf t) => Nullable (Queue t)

instance Monoid (ContainerOf t a) => Monoid (Queue t a) where
    mempty       = Queue mempty
    mappend q q' = Queue $ mappend (unQueue q) (unQueue q')

-- utils

pop_ :: IsQueue t => Queue t a -> Maybe (Queue t a)
pop_ = fmap snd . pop

unsafePop_ :: IsQueue t => Queue t a -> Queue t a
unsafePop_ = fromJust . pop_


----------------------------------------------------------------------
-- Specific queue implementations
----------------------------------------------------------------------

-- FIFO

data FIFO = FIFO deriving (Show)
type Fifo = Queue FIFO

type instance ContainerOf FIFO = [] 
instance IsQueue FIFO where
    --null   (Queue c) = null c
    push a (Queue c) = Queue $ a:c
    pop    (Queue c) = case c of
        []     -> Nothing
        (a:as) -> Just (a, Queue as)

-- LIFO

data LIFO = LIFO deriving (Show)
type Lifo = Queue LIFO

type instance ContainerOf LIFO = Seq
instance IsQueue LIFO where
    push a (Queue c) = Queue $ c Seq.|> a
    pop    (Queue c) = if null c then Nothing
                       else Just $ (toList h !! 0, Queue t)
                       where (h,t) = Seq.splitAt 1 c


-- FINO

data FINO = FINO deriving (Show)
type Fino = Queue FINO

type instance ContainerOf FINO = []
instance IsQueue FINO where
    push  _ = id
    pop   _ = Nothing



main = do
    let q = pop
          $ push 6
          $ push 5
          $ (mempty :: Lifo Int)
    print q
    return ()