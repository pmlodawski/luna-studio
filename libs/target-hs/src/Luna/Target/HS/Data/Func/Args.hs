---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}

!{-# LANGUAGE RightSideContexts #-}

module Luna.Target.HS.Data.Func.Args where
    
import GHC.TypeLits
import Data.Typeable (Typeable)
import Flowbox.Utils
import Data.Typeable (Proxy)

----------------------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------------------

data    Unprovided                      = Unprovided  deriving (Show, Eq, Typeable)
newtype Provided   a                    = Provided a  deriving (Show, Eq, Typeable, Functor)
newtype Default    a                    = Default a   deriving (Show, Eq, Typeable, Functor)

newtype Unnamed    arg                  = Unnamed arg deriving (Show, Eq, Typeable, Functor)
newtype Untyped    arg                  = Untyped arg deriving (Show, Eq, Typeable, Functor)
newtype Named      (name :: Symbol) arg = Named   arg deriving (Show, Eq, Typeable, Functor)
newtype Typed      a arg                = Typed   arg deriving (Show, Eq, Typeable, Functor)


----------------------------------------------------------------------------------
-- Params
----------------------------------------------------------------------------------

type Param             = Unnamed    (Untyped Unprovided)
type DParam   a        = Unnamed    (Untyped (Default a))
type TParam   t        = Unnamed    (Typed t Unprovided)
type NParam   name     = Named name (Untyped Unprovided)
type NDParam  name a   = Named name (Untyped (Default a))
type NTParam  name t   = Named name (Typed t Unprovided)
type TDParam  t a      = Unnamed    (Typed t (Default a))
type NTDParam name t a = Named name (Typed t (Default a))


----------------------------------------------------------------------------------
-- MkArg
----------------------------------------------------------------------------------

class MkArg a where
    mkArg :: a

instance MkArg (Named name arg)        <= MkArg arg            where mkArg = Named   mkArg
instance MkArg (Typed t arg)           <= MkArg arg            where mkArg = Typed   mkArg
instance MkArg (Unnamed arg)           <= MkArg arg            where mkArg = Unnamed mkArg
instance MkArg (Untyped arg)           <= MkArg arg            where mkArg = Untyped mkArg
instance MkArg Unprovided                                      where mkArg = Unprovided

instance MkArg (val -> Named name arg) <= (MkArg (val -> arg)) where mkArg = Named   . mkArg
instance MkArg (val -> Typed t arg)    <= (MkArg (val -> arg)) where mkArg = Typed   . mkArg
instance MkArg (val -> Unnamed arg)    <= (MkArg (val -> arg)) where mkArg = Unnamed . mkArg
instance MkArg (val -> Untyped arg)    <= (MkArg (val -> arg)) where mkArg = Untyped . mkArg
instance MkArg (val -> Provided val')  <= (val~val')           where mkArg = Provided
instance MkArg (val -> Default val')   <= (val~val')           where mkArg = Default

----------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------

instance Monad Unnamed where
    return = Unnamed
    (Unnamed a) >>= f = f a

instance Monad Untyped where
    return = Untyped
    (Untyped a) >>= f = f a

instance Monad (Named name) where
    return = Named
    (Named a) >>= f = f a

instance Monad (Typed a) where
    return = Typed
    (Typed a) >>= f = f a


----------------------------------------------------------------------------------
-- AppArg
----------------------------------------------------------------------------------

class AppArg val arg out | val arg -> out where
    appArg :: val -> arg -> out

instance AppArg val (Named name arg) (Named name (rarg)) <= (AppArg val arg rarg) where
    appArg val arg = fmap (appArg val) arg

instance AppArg val (Unnamed arg) (Unnamed (rarg)) <= (AppArg val arg rarg) where
    appArg val arg = fmap (appArg val) arg

instance AppArg val (Typed t arg) (Typed t (Provided val)) <= (t~val) where
    appArg val _ = Typed $ Provided val

instance AppArg val (Untyped arg) (Untyped (Provided val)) where
    appArg val _ = Untyped $ Provided val


----------------------------------------------------------------------------------
-- AppNextArg
----------------------------------------------------------------------------------

class AppNextArg val args out | val args -> out where
    appNextArg :: val -> args -> out

instance AppNextArg val (n(t Unprovided), args) (rarg, args) <= (AppArg val (n(t Unprovided)) rarg) where
    appNextArg val (arg, args) = (appArg val arg, args)

instance AppNextArg val (n(t(Default a)), args) (rarg, args) <= (AppArg val (n(t(Default a))) rarg) where
    appNextArg val (arg, args) = (appArg val arg, args)

instance AppNextArg val (n(t(Provided a)), args) (n(t(Provided a)), out) <= (AppNextArg val args out) where
    appNextArg val (a, args) = (a, appNextArg val args)



----------------------------------------------------------------------------------
-- AppArgByName
----------------------------------------------------------------------------------

class AppArgByName (name :: Symbol) val args out | name val args -> out where
    appArgByName :: Proxy name -> val -> args -> out

instance AppArgByName name val (Named name arg, args) (Named name rarg, args) <= (AppArg val arg rarg) where
    appArgByName _ val (arg, args) = (appArg val arg, args)

instance AppArgByName name val (a, args) (a', out) <= (a~a', AppArgByName name val args out) where
    appArgByName name val (a, args) = (a, appArgByName name val args)


----------------------------------------------------------------------------------
-- ReadArgs
----------------------------------------------------------------------------------

class ReadArgs args vals | args -> vals where
    readArgs :: args -> vals

instance ReadArgs (arg, args) (rarg, rargs) <= (ReadArg arg rarg, ReadArgs args rargs) where
    readArgs (arg, args) = (readArg arg, readArgs args)

instance ReadArgs () () where
    readArgs = id


----------------------------------------------------------------------------------
-- ReadArg
----------------------------------------------------------------------------------

class ReadArg arg val | arg -> val where
    readArg :: arg -> val

instance ReadArg (Named name arg) rarg <= (ReadArg arg rarg) where
    readArg (Named arg) = readArg arg

instance ReadArg (Typed a arg) rarg <= (ReadArg arg rarg) where
    readArg (Typed arg) = readArg arg

instance ReadArg (Unnamed arg) rarg <= (ReadArg arg rarg) where
    readArg (Unnamed arg) = readArg arg

instance ReadArg (Untyped arg) rarg <= (ReadArg arg rarg) where
    readArg (Untyped arg) = readArg arg

instance ReadArg (Provided a) a where
    readArg (Provided a) = a

instance ReadArg (Default a) a where
    readArg (Default a) = a


----------------------------------------------------------------------------------
-- AllArgs
----------------------------------------------------------------------------------

class AllArgs args (out :: Bool) | args -> out

instance AllArgs ()                         True
instance AllArgs (n(t(Provided val)), args) out  <= AllArgs args out
instance AllArgs (a, args)                  out  <= out~False

