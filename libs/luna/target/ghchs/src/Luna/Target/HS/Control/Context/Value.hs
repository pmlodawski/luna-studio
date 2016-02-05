---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}


module Luna.Target.HS.Control.Context.Value where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Shuffle
import           Control.PolyApplicative
import           Control.PolyMonad
import           Data.Typeable           (Typeable)
import           Data.TypeLevel
import           Flowbox.Utils

--------------------------------------------------------------------------------
-- Structures
--------------------------------------------------------------------------------

newtype Value m s v = Value (m (s v)) deriving (Typeable, Functor)

fromValue (Value a) = a

withValue f (Value a) = Value $ f a

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

-- FIXME [wd]: to remove?
class LiftValue m t where
    liftValue :: Functor s => Value m s a -> t s a


class LiftValue' m s t where
    liftValue' :: m (s :: * -> *) a -> t m s a

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Show (m (s a)) => Show (Value m s a) where
#ifdef DEBUG
    show (Value a) = "Value (" ++ child ++ ")" where
        child = show a
        content = if ' ' `elem` child then "(" ++ child ++ ")" else child
#else
    show (Value a) = show a
#endif

-- FIXME[wd]: do we need this? How to implement Monad for Value?
--instance (Monad m, Monad s) => Monad (Value m s) where
--    return = Value . return . return
--    Value m >>= Value f = Value $ do
--        s <- m
--        shuffle $ do
--            v <- s
--            shuffle . fromValue $ f v

---

