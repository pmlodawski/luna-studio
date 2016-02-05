{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

--{-# LANGUAGE IncoherentInstances #-}

{-# LANGUAGE DeriveDataTypeable        #-}


{-# LANGUAGE DysfunctionalDependencies #-}


{-# LANGUAGE RebindableSyntax          #-}

module TstX where

import           GHC.Prim                                  (Constraint)

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Morph
import           Control.Monad.Trans
import           Control.PolyApplicative
import           Control.PolyApplicative.App
import           Data.TupleList
import           Data.Typeable                             (Proxy (..), Typeable)
import           Data.TypeLevel
import           Data.Wrap
import           Flowbox.Utils
import           GHC.TypeLits                              (Symbol)
import           Luna.Target.HS.Control.Context
import           Luna.Target.HS.Control.Context.Pipe3      hiding (main)
import           Luna.Target.HS.Control.Context.Rebindable
import           Luna.Target.HS.Control.Error
import           Luna.Target.HS.Control.Flow
import           Luna.Target.HS.Control.Flow.Utils
import           Luna.Target.HS.Data.Func
import           Luna.Target.HS.Data.Struct.Prop
import           Luna.Target.HS.Utils.BaseMonads

class Pipe3 m1 m2 where
    pipe3 :: (m1 a -> b) -> m2 a -> b





class Pipe4 a b c | a b -> c where
    pipe4 :: a -> b -> c


instance  (func~(m1 a1 -> b), a1~a2, m1 ~ MonadCtx envout set2 m2) =>Pipe4 func (MonadCtx envout set2 m2 a2) b  where
    pipe4 = ($)

instance  (func~(m1 a1 -> b), a1~a2, m1 ~ Value m2) =>Pipe4 func (Value m2 a2) b  where
    pipe4 = ($)


instance  (a1~a2, env1~env2, m1~m2, set1~set2) =>Pipe4 (Req req (MonadCtx env1 set1 m1) a1 -> b) (MonadCtx env2 set2 m2 a2) b  where
    pipe4 f = f . Req

instance  (a1~a2, req~req2, env1~env2, m1~m2, set1~set2) =>Pipe4 (Req req (MonadCtx env1 set1 m1) a1 -> b) (Req req2 (MonadCtx env2 set2 m2) a2) b  where
    pipe4 = ($)

instance  (a1~a2, env~m2, set~Insert req Empty, m1~t m2, MonadTrans t, Monad m2) =>Pipe4 (Req req (MonadCtx env set m1) a1 -> b) (Value m2 a2) b  where
    pipe4 f = f . Req . MonadCtx . lift . fromValue





class AppMonadCtx a b c | a b -> c where
    appMonadCtx :: a -> b -> c

instance  (env1~env2, set1~set2, m1~m2, a1~a2) =>AppMonadCtx (Req req (MonadCtx env1 set1 m1) a1 -> b) (MonadCtx env2 set2 m2 a2) b  where
    appMonadCtx f = f . Req

instance  (req1~req2, env1~env2, set1~set2, m1~m2, a1~a2) =>AppMonadCtx (Req req1 (MonadCtx env1 set1 m1) a1 -> b) (Req req2 (MonadCtx env2 set2 m2) a2) b  where
    appMonadCtx = ($)

instance  (a1~Value Pure a2, env~m2, set~Insert req Empty, m1~t m2, MonadTrans t, Monad m2, Functor m2) =>AppMonadCtx (Req req (MonadCtx env set m1) a1 -> b) (Value m2 a2) b  where
    appMonadCtx f = f . Req . MonadCtx . lift . (fmap (Value . Pure)) . fromValue


--class AppMonadCtx2 a b | a -> b where -- | a -> b where
--    appMonadCtx2 :: a -> b

--instance AppMonadCtx2 (MonadCtx env set m a) (Req req (MonadCtx env set m) a) where
--    appMonadCtx2 = Req

--instance AppMonadCtx2 (Req req (MonadCtx env set m) a) (Req req (MonadCtx env set m) a) where
--    appMonadCtx2 = id

--instance  (a1~Value Pure a2, env~m2, set~Insert req Empty, m1~t m2, MonadTrans t, Monad m2, Functor m2) =>AppMonadCtx2 (Value m2 a2) (Req req (MonadCtx env set m1) a1)  where
--    appMonadCtx2 = Req . MonadCtx . lift . (fmap (Value . Pure)) . fromValue

---------

class AppMonadCtx2 a b | a -> b where -- | a -> b where
    appMonadCtx2 :: a -> b

instance AppMonadCtx2 (MonadCtx env set m a) (Req req (MonadCtx env set m) a) where
    appMonadCtx2 = Req


instance AppMonadCtx2 (Req req (MonadCtx env set m) a) (Req req (MonadCtx env set m) a) where
    appMonadCtx2 = id

instance  (a1~Value Pure a2, env~m2, set~Insert req Empty, m1~t m2, MonadTrans t, Monad m2, Functor m2) =>AppMonadCtx2 (Value m2 a2) (Req req (MonadCtx env set m1) a1)  where
    appMonadCtx2 = Req . MonadCtx . lift . (fmap (Value . Pure)) . fromValue


--class AppMonadCtx2 x y | x -> y where
--    f :: x -> (forall req. MkReq req y)

--type family MkReq req y where
--    MkReq req (MonadCtx env set m) = Req req (MonadCtx env set m)

