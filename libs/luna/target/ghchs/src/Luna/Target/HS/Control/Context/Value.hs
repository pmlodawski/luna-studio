---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
!{-# LANGUAGE RightSideContexts #-}

module Luna.Target.HS.Control.Context.Value where

import Control.PolyMonad
import Control.PolyApplicative
import Luna.Target.HS.Control.Context.Env
import Control.Monad.IO.Class
import Data.Typeable (Typeable)
import Flowbox.Utils
import Data.TypeLevel
import Control.Applicative

--------------------------------------------------------------------------------
-- Structures
--------------------------------------------------------------------------------

newtype Value m v = Value (m v) deriving (Typeable)

fromValue (Value a) = a


--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

class LiftValue m mout where
    liftValue :: Value m a -> mout a


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Show (m a) => Show (Value m a) where
#ifdef DEBUG
    show (Value a) = "Value (" ++ child ++ ")" where
        child = show a
        content = if ' ' `elem` child then "(" ++ child ++ ")" else child
#else
    show (Value a) = show a
#endif

---

instance Monad m => Monad (Value m) where
    return = Value . return
    Value ma >>= f = Value $ do
        a <- ma
        fromValue $ f a

instance Functor m => Functor (Value m) where
    fmap f (Value a) = Value $ fmap f a

instance (Functor m, Monad m) => Applicative (Value m) where
    pure  = Value . return
    (Value mf) <*> (Value ma) = Value $ do
        f <- mf
        a <- ma
        return $ f a

---

instance LiftValue IO m <= MonadIO m where
    liftValue = liftIO . fromValue

instance LiftValue Pure m <= Monad m where
    liftValue = return . fromPure . fromValue