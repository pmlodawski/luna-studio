---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Wrapper where

import Control.Monad.Trans
import Prelude
import Control.Lens hiding (Wrapped)

--FIXME[wd]: change name from Wrapp* to , because it overlaps with Lens' Wrapped
-- or leave them as is, because are sometimes more powerful


----------------------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------------------

class Wrap m where
    wrap :: a -> m a

    default wrap :: Monad m => a -> m a
    wrap = return

class Unwrap m where
    unwrap :: m a -> a

class (Wrap m, Unwrap m) => Wrapper m

class WrapT t where
    wrapT :: m a -> t m a

    default wrapT :: (MonadTrans t, Monad m) => m a -> t m a
    wrapT = lift

class UnwrapT t where
    unwrapT :: t m a -> m a

class (WrapT t, UnwrapT t) => WrapperT t

class Wrapped a where
    wrapped :: Lens (a t) (a t') t t'
    default wrapped :: (Wrap a, Unwrap a) => Lens (a t) (a t') t t'
    wrapped = lens unwrap (const wrap)


----------------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------------

rewrap :: (Unwrap m, Wrap n) => m a -> n a
rewrap = wrap . unwrap

--wrapped = "d"
--wrapped :: Wrapper m => Lens (m a) (m b) a b
--wrapped = lens unwrap (const wrap)

----------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------

instance {-# OVERLAPPABLE #-} (Wrap a, Unwrap a) => Wrapper a
instance {-# OVERLAPPABLE #-} (WrapT a, UnwrapT a) => WrapperT a