{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

--{-# LANGUAGE IncoherentInstances #-}

{-# LANGUAGE DeriveDataTypeable #-}

!{-# LANGUAGE RightSideContexts #-}
{-# LANGUAGE DysfunctionalDependencies #-}


{-# LANGUAGE RebindableSyntax #-}

module TstX where

import GHC.Prim (Constraint)

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.PolyApplicative
import Control.PolyApplicative.App
import Luna.Target.HS.Control.Context
import Luna.Target.HS.Control.Error
import Luna.Target.HS.Control.Flow
import Luna.Target.HS.Utils.BaseMonads
import Luna.Target.HS.Data.Func
import Control.Monad.Morph
import Flowbox.Utils
import Data.Typeable (Typeable, Proxy(..))
import Data.TypeLevel
import Data.Wrap
import Luna.Target.HS.Data.Struct.Prop
import Luna.Target.HS.Control.Context.Rebindable
import GHC.TypeLits (Symbol)
import Data.TupleList
import Luna.Target.HS.Control.Flow.Utils
import Luna.Target.HS.Control.Context.Pipe3 hiding (main)

class Pipe3 m1 m2 where
    pipe3 :: (m1 a -> b) -> m2 a -> b





class Pipe4 a b c | a b -> c where
    pipe4 :: a -> b -> c


instance Pipe4 func (MonadCtx envout set2 m2 a2) b <= (func~(m1 a1 -> b), a1~a2, m1 ~ MonadCtx envout set2 m2) where
    pipe4 = ($)

instance Pipe4 func (Value m2 a2) b <= (func~(m1 a1 -> b), a1~a2, m1 ~ Value m2) where
    pipe4 = ($)


instance Pipe4 (Req req (MonadCtx env1 set1 m1) a1 -> b) (MonadCtx env2 set2 m2 a2) b <= (a1~a2, env1~env2, m1~m2, set1~set2) where
    pipe4 f = f . Req

instance Pipe4 (Req req (MonadCtx env1 set1 m1) a1 -> b) (Req req2 (MonadCtx env2 set2 m2) a2) b <= (a1~a2, req~req2, env1~env2, m1~m2, set1~set2) where
    pipe4 = ($)

instance Pipe4 (Req req (MonadCtx env set m1) a1 -> b) (Value m2 a2) b <= (a1~a2, env~m2, set~Insert req Empty, m1~t m2, MonadTrans t, Monad m2) where
    pipe4 f = f . Req . MonadCtx . lift . fromValue





class AppMonadCtx a b c | a b -> c where
    appMonadCtx :: a -> b -> c

instance AppMonadCtx (Req req (MonadCtx env1 set1 m1) a1 -> b) (MonadCtx env2 set2 m2 a2) b <= (env1~env2, set1~set2, m1~m2, a1~a2) where
    appMonadCtx f = f . Req

instance AppMonadCtx (Req req1 (MonadCtx env1 set1 m1) a1 -> b) (Req req2 (MonadCtx env2 set2 m2) a2) b <= (req1~req2, env1~env2, set1~set2, m1~m2, a1~a2) where
    appMonadCtx = ($)

instance AppMonadCtx (Req req (MonadCtx env set m1) a1 -> b) (Value m2 a2) b <= (a1~Value Pure a2, env~m2, set~Insert req Empty, m1~t m2, MonadTrans t, Monad m2, Functor m2) where
    appMonadCtx f = f . Req . MonadCtx . lift . (fmap (Value . Pure)) . fromValue


--class AppMonadCtx2 a b | a -> b where -- | a -> b where
--    appMonadCtx2 :: a -> b

--instance AppMonadCtx2 (MonadCtx env set m a) (Req req (MonadCtx env set m) a) where
--    appMonadCtx2 = Req

--instance AppMonadCtx2 (Req req (MonadCtx env set m) a) (Req req (MonadCtx env set m) a) where
--    appMonadCtx2 = id

--instance AppMonadCtx2 (Value m2 a2) (Req req (MonadCtx env set m1) a1) <= (a1~Value Pure a2, env~m2, set~Insert req Empty, m1~t m2, MonadTrans t, Monad m2, Functor m2) where
--    appMonadCtx2 = Req . MonadCtx . lift . (fmap (Value . Pure)) . fromValue

---------

class AppMonadCtx2 a b | a -> b where -- | a -> b where
    appMonadCtx2 :: a -> b

instance AppMonadCtx2 (MonadCtx env set m a) (Req req (MonadCtx env set m) a) where
    appMonadCtx2 = Req


instance AppMonadCtx2 (Req req (MonadCtx env set m) a) (Req req (MonadCtx env set m) a) where
    appMonadCtx2 = id

instance AppMonadCtx2 (Value m2 a2) (Req req (MonadCtx env set m1) a1) <= (a1~Value Pure a2, env~m2, set~Insert req Empty, m1~t m2, MonadTrans t, Monad m2, Functor m2) where
    appMonadCtx2 = Req . MonadCtx . lift . (fmap (Value . Pure)) . fromValue


--class AppMonadCtx2 x y | x -> y where 
--    f :: x -> (forall req. MkReq req y)

--type family MkReq req y where
--    MkReq req (MonadCtx env set m) = Req req (MonadCtx env set m)

