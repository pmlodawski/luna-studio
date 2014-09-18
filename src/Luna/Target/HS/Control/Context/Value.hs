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
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
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

newtype Value m s v = Value (m (s v)) deriving (Typeable, Functor)

fromValue (Value a) = a

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

---

type family EnvMerge3 a b where
  EnvMerge3 (Value Pure) (Value Pure) = (Value Pure)
  EnvMerge3 a             b             = (Value IO)