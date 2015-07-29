---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}


module Type.Infer where

import Data.Typeable
import Data.Proxy.Utils
import Unsafe.Coerce
import Prelude

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

class InferType a where
    inferType :: Proxy a -> Proxy a
    inferType = undefined

class InferType2 (a::k) (b::k) | a -> b where
    inferType2 :: Proxy a -> Proxy b
    inferType2 = undefined


class BreakInference a b | a -> b where
    breakInference :: a -> b

--instance  (a~b) =>BreakInference a b  where
--    breakInference a = a

instance BreakInference Int String where
    breakInference = undefined

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

--instance  InferType2 a a=>Num a  
--instance  InferType2 a a=>Monad a  

--instance InferType2 Int Int

--instance InferType2 (m a) (m' a') <= (InferType2 m m', InferType2 a a')
--instance InferType2 (a :: *) b <= (b~Id0)
--instance InferType2 (a :: * -> *) b <= (b~Id1)
--instance InferType2 (a :: * -> * -> *) b <= (b~Id2)


instance InferType a => Num a
instance (InferType a, Applicative a) => Monad a

instance InferType Int

instance (InferType m, InferType a) => InferType (m a)
instance (a~Id0)                    => InferType (a :: *)
instance (a~Id1)                    => InferType (a :: * -> *)
instance (a~Id2)                    => InferType (a :: * -> * -> *)


