{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Typeable

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

class InferType a where
    inferType :: Proxy a -> Proxy a
    inferType = undefined

--------------------------------------------------------------------------------
-- Proxy datatypes
--------------------------------------------------------------------------------

data Id0 = Id0 deriving (Show, Typeable)
data Id1 t1 = Id1 deriving (Show, Typeable)
data Id2 t1 t2 = Id2 deriving (Show, Typeable)
data Id3 t1 t2 t3 = Id3 deriving (Show, Typeable)
data Id4 t1 t2 t3 t4 = Id4 deriving (Show, Typeable)
data Id5 t1 t2 t3 t4 t5 = Id5 deriving (Show, Typeable)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance InferType Int 

instance (InferType m, InferType a) => InferType (m a)
instance (a~Id0)                    => InferType (a :: *)
instance (a~Id1)                    => InferType (a :: * -> *)
instance (a~Id2)                    => InferType (a :: * -> * -> *)

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------
toProxy :: a -> Proxy a
toProxy _ = Proxy

inferTypeBase :: a -> Proxy a
inferTypeBase a = inferType $ toProxy a

instance InferType Foo1 
instance InferType Foo2  

tm _ = 5

data Foo1 a = Foo1 a deriving (Show, Typeable)
data Foo2 a b = Foo2 a b deriving (Show, Typeable)

instance Monad Id1 -- dummy instances just for tests
instance Num Id0


main = do
    print $ typeOf $ inferType $ toProxy $ (return (5))
    print $ typeOf $ inferType $ toProxy $ (5)
    print $ typeOf $ inferType $ toProxy $ (Foo1 (return 5))
    print $ typeOf $ inferType $ toProxy $ (Foo2 (return 5) (return 5))

    print "hello"

---- OUTPUT:
--    Proxy (Id1 Id0)
--    Proxy Id0
--    Proxy (Foo1 (Id1 Id0))
--    Proxy (Foo2 (Id1 Id0) (Id1 Id0))
--    "hello"