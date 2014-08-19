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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}
!{-# LANGUAGE RightSideContexts #-}

--{-# LANGUAGE DysfunctionalDependencies #-}


module Luna.Target.HS.Control.Context.Pipe2 where

import Control.PolyMonad
import Control.Applicative
import Control.PolyApplicative
import Luna.Target.HS.Control.Context.Env
import Luna.Target.HS.Control.Context.Value
import Luna.Target.HS.Control.Context.Monad
import Luna.Target.HS.Control.Context.App
import Data.TypeLevel
import Control.Monad.IO.Class
import Control.PolyApplicative.App
import Luna.Target.HS.Utils.BaseMonads
import Data.Typeable
import Luna.Target.HS.Control.Context.Bind
import Control.Monad.Trans.Class (MonadTrans, lift)

--import Control.Monad (join)

----------------------------------------------------------------------------------
---- Type classes
----------------------------------------------------------------------------------

--class Pipe a b c | a b -> c where
--    pipe :: a -> b -> c

----------------------------------------------------------------------------------
---- Instances
----------------------------------------------------------------------------------

--class LiftValue m mout where
--    liftValue :: Value m a -> mout a

--instance LiftValue IO m <= MonadIO m where
--    liftValue = liftIO . fromValue

--instance LiftValue Pure m <= Monad m where
--    liftValue = return . fromPure . fromValue


----class LiftCtx m mout where
----    liftCtx :: m a -> mout a

----instance LiftCtx IO m <= MonadIO m where
----    liftCtx = liftIO

----instance LiftCtx Pure m <= Monad m where
----    liftCtx = return . fromPure


--class LiftCtx m where
--    liftCtx :: MonadIO mout => m a -> mout a

--instance LiftCtx IO where
--    liftCtx = liftIO

--instance LiftCtx Pure where
--    liftCtx = return . fromPure


----instance Pipe (Value m1 a1 -> b) (Value m2 a2) b <= (a1~a2, m1~m2) where
----    pipe = ($)

----instance Pipe (m1 a1 -> b) (Value m2 a2) b <= (a1~a2, m1~(Value m2)) where
----    pipe = ($)

----instance Pipe (Value m1 a1 -> b) (Value m2 a2) b <= (a1~a2, m1~m2) where
----    pipe = ($)

------instance Pipe (MonadCtx env1 set m1 a1 -> b) (MonadCtx env2 set m2 a2) b <= (env1~env2, m1~m2, a1~a2) where
------    pipe = ($)

----instance Pipe (MonadCtx env1 set1 m1 a1 -> b) (MonadCtx env2 set2 m2 a2) b <= (env1~env2, m1~m2, a1~a2, set1~set2) where
----    pipe = ($)

--instance Pipe (Value m1 a1 -> mb b) (Value m2 a2) out <= (out~mb b, a1~a2, m1~m2) where
--    pipe = ($)




--instance Pipe (Req req (MonadCtx env1 set1 m1) a1 -> mb b) (MonadCtx env2 set2 m2 a2) out <= (out~mb b, env1~env2, m1~m2, a1~a2, set1~set2) where
--    pipe f = f . Req


----instance Pipe (MonadCtx env (ConstrainSet req set) m1 a1 -> b) (Value m2 a2) b <= (env~m2, a1~a2, LiftValue m2 m1) where
----    pipe f a = f . MonadCtx $ liftValue a

--instance Pipe (Req req (MonadCtx env set m1) a1 -> mb b) (Value m2 a2) out <= (out~mb b, a1~a2, set~Insert req Empty, m1~t m2, MonadTrans t, Monad m2) where
--    pipe f = f . matchReqMonadCtx . lift . fromValue


--    --instance Pipe (Value m1 a1 -> mb b) (MonadCtx envout set2 m2 a2) (MonadCtx env2 set2 mout b) <= (a1~a2, m1~Pure, PolyMonad (MonadCtx envout set2 m2) mb (MonadCtx env2 set2 mout)) where
--    --    pipe f a = a >>=~ fn where
--    --        fn = f . Value . Pure

--instance Pipe (Value m1 a1 -> (b -> c)) (MonadCtx env2 set2 m2 a2) out <= (out~(MonadCtx env2 set2 m2 (b -> c)), m1~Pure, a1~a2, Functor m2) where
--    pipe f a = fmap (f . Value . Pure) a


--instance Pipe (MonadCtx env set m (m1 a1 -> (b -> c))) (Value m2 a2) out <= (out~(MonadCtx env set m (b -> c)), m1~Value m2, a1~a2, Functor m) where
--    pipe mf a = fmap ($ a) mf


----instance Pipe (MonadCtx env set m (m1 a1 -> b)) (Value m2 a2) out <= (out~(MonadCtx env set m b), m1~Value m2, a1~a2, Functor m) where
----    pipe mf a = fmap ($ a) mf


--instance Pipe (MonadCtx env set m (ma a1 -> mb b)) (Value m2 a2) out <= (out~mout b, ma~Value m2, a1~a2, Functor m, PolyJoin (MonadCtx env set m) mb mout) where
--    pipe mf a = polyJoin $ fmap ($ a) mf


--    --instance Pipe (MonadCtx env1 set1 m1 (ma a -> mb b)) (MonadCtx env2 set2 m2 a2) out <= (out~mout b, m1~Value m2, a1~a2, Functor m, PolyJoin (MonadCtx env set m) mb mout) where
--    --    pipe mf a = polyJoin $ fmap ($ a) mf




--instance Pipe (Value m1 a1 -> mb b) (MonadCtx env2 set2 m2 a2) out <= (out~mout b, m1~Pure, a1~a2, PolyMonad (MonadCtx env2 set2 m2) mb mout) where
--    pipe f a = a >>=~ fn where
--        fn = f . Value . Pure

----to wyzej dopracowac dla dalszych funkcji

--instance Pipe (m1 a1 -> mb b) (MonadCtx envout set2 m2 a2) out <= (out~mb b, a1~a2, m1 ~ MonadCtx envout set2 m2) where
--    pipe = ($)

--instance Pipe (m1 a1 -> mb b) (Value m2 a2) out <= (out~mb b, a1~a2, m1 ~ Value m2) where
--    pipe = ($)


--class PolyJoin m1 m2 m3 | m1 m2 -> m3 where
--    polyJoin :: m1 (m2 a) -> m3 a

--instance PolyJoin Pure Pure Pure where
--    polyJoin (Pure a) = a

--instance PolyJoin Pure IO IO where
--    polyJoin (Pure a) = a

--instance PolyJoin IO Pure IO where
--    polyJoin = fmap fromPure

--instance PolyJoin IO IO IO where
--    polyJoin = join

--instance PolyJoin (Value m1) (Value m2) (Value m3) <= (Functor m1, PolyJoin m1 m2 m3) where
--    polyJoin = withValue $ polyJoin . fmap fromValue


--instance PolyJoin (Value m1) (MonadCtx env2 set2 m2) (MonadCtx env2 set2 m2) <= (Monad m2, Functor m2, LiftValue m1 m2) where
--    polyJoin = MonadCtx . join . fmap fromMonadCtx . liftValue


--instance PolyJoin (MonadCtx env1 set1 m1) (Value m2) (MonadCtx env1 set1 m1) <= (Monad m1, Functor m1, LiftValue m2 m1) where
--    polyJoin = withMonadCtx $ join . fmap liftValue


--instance PolyJoin (MonadCtx env1 set1 m1) (MonadCtx env2 set2 m2) (MonadCtx envout setout m1) <= (m1~m2, Monad m1, Functor m1, envout ~ EnvMerge env1 env2, setout ~ Union set1 set2) where
--    polyJoin = withMonadCtx $ join . fmap fromMonadCtx

--withValue    f = Value . f . fromValue
--withMonadCtx f = MonadCtx . f . fromMonadCtx

----instance Pipe (Req req (MonadCtx env set m1) a1 -> b) (Value m2 a2) b <= (a1~a2, set~Insert req Empty, m1~t m2, MonadTrans t, Monad m2) where
----    pipe f a = f (matchReqMonadCtx $ (lift . fromValue) a)

----instance Pipe (MonadCtx env (ConstrainSet req set) m1 a1 -> b) (Value m2 a2) b <= (a1~a2, req~set, m1~t m2, MonadTrans t, Monad m2) where
----    pipe f a = f (matchReqMonadCtx $ (lift . fromValue) a)


----instance Pipe (MonadCtx env set m2 a2 -> b) (Value m1 a1) b <= (a1~a2, env~m1, LiftValue m1 m2, Monad m1) where
----    pipe f a = f . MonadCtx $ liftValue a

------ dziala dla (!) addMeSingleS' >>> getX (?)
------instance Pipe (MonadCtx env1 set1 m1 a1 -> mout b) (MonadCtx env2 set2 m2 a2) (MonadCtx env1 setout mout b) <= (env1~env2, m1~m2, a1~a2, setout ~ Difference set2 set1) where
------    pipe f a = MonadCtx (f . MonadCtx $ fromMonadCtx a)

----instance Pipe (MonadCtx env1 set1 m1 a1 -> Value mb b) (MonadCtx env2 set2 m2 a2) (MonadCtx env1 setout mb b) <= (env1~env2, m1~m2, a1~a2, setout ~ Difference set2 set1) where
----    pipe = undefined

--(>>>) = pipe


--liftf0 = Value . Pure
--liftf1 = app1 . Value . Pure
--liftf2 = app2 . Value . Pure


----liftf1x = app1 . Value . app1 . Pure

----addMe :: Int -> Int -> Int
--addMe = (+)

--addMeSingle :: Int -> Int
--addMeSingle a = a + a

--addMe' :: (PolyApplicative (Value Pure) m5 m1, PolyApplicative m1 m2 m3, Num b) => m5 b -> m2 b -> m3 b
--addMe' = liftf2 addMe

--addMe'' :: (PolyApplicative Pure ma out, PolyApplicative out mb out1, Num a) => Value ma a -> Value mb a -> Value out1 a 
--addMe'' = liftf2 addMe

--addMeSingle' a = liftf1 addMeSingle a

--addMeSingle'' :: PolyApplicative (Value Pure) (Value m2) m3 => Value m2 Int -> m3 Int
--addMeSingle'' = liftf1 addMeSingle

--addMeSingle'2 (a :: Value base Int) = liftf1 addMeSingle a

----addMeSingle'2 a = Value $ addMeSingle' a
--addMeSingleS' (a :: MonadCtx env set (StateT Int mb) Int ) = addMeSingle' $ fmap fst $ runStateTX a (0::Int)

----addMeSingleS'2 (a :: MonadCtx env set (StateT Int mb) Int ) = addMeSingle'2 >>> (fmap fst $ runStateTX a (0::Int))

----instance MonadIO Pure where
----    liftIO = undefined

--        --runStateTX
--        --  :: MatchMonadCloseProto
--        --       (IsEmpty (Remove (Proxy StateT) set))
--        --       (MonadCtx env (Remove (Proxy StateT) set) mb)
--        --       t =>
--        --     MonadCtx env set (StateT a mb) a1 -> a -> t (a1, a)
--        --- trzeba zmienic "set" z "MonadCtx env set [...]" na cos co mowi co MUSI byc spelnione!
--        --- aby dzialalo: (flip runStateTX (0::Int)) >>> (Value $ Pure 1)

----matchReqMonadCtx :: m val -> MonadCtx base (ConstrainSet set set) m val
----matchReqMonadCtx = MonadCtx

--matchReqMonadCtx :: m val -> Req req (MonadCtx base (Insert req Empty) m) val
--matchReqMonadCtx = Req . MonadCtx


--main = do
--    let x = return 5 :: Value Pure Int

    
--    print $ addMeSingle'' >>> (Value $ Pure 1)
--    print $ addMeSingle' >>> (Value $ Pure 1)

--    print $ addMe'' >>> (Value $ Pure 1) >>> (Value $ Pure 4)
--    print $ addMe' >>> (Value $ Pure 1) >>> (Value $ Pure 4)
--    ---
--    print $ (flip runStateTX' (0::Int)) >>> getX -- (Value $ Pure 1)
--    print $ (flip runStateTX' (0::Int)) >>> ((flip runStateTX' (0::Int)) >>> (Value $ Pure 1) )
--    print $ (flip runStateTX' (0::Int)) (matchReqMonadCtx $ (lift . fromValue) (Value $ Pure 1))

--    print $ (flip runStateTX (0::Int)) $ addMeSingle'' >>> getX
    
--    print $ (flip runStateTX (0::Int)) $ addMeSingle' >>> getX


--    print $ (flip runStateTX (0::Int)) $ addMe' >>> getX >>> getX

--    print $ (flip runStateTX (0::Int)) $ addMe'' >>> (Value $ Pure 1) >>> getX

--    print $ (flip runStateTX (0::Int)) $ addMe'' >>> getX >>> (Value $ Pure 1)
--    --print $ (flip runStateTX (0::Int)) $ addMe'' >>> getX >>> getX
--    --print $ add



-----
--        --print $ (flip runStateTX' (0::Int)) $ removeReqConstrains (MonadCtx $ (lift . fromValue) (Value $ Pure (1::Int)) :: MonadTrans t => MonadCtx Pure (ConstrainSet () (Proxy StateT, ())) (t Pure) Int)
--        --print $ (flip runReaderTX' (0::Int)) $ removeReqConstrains  $ (flip runStateTX' (0::Int)) $ removeReqConstrains $ (MonadCtx $ (lift . lift . fromValue) (Value $ Pure (1::Int)) :: (MonadTrans t0, MonadTrans t1, Monad (t1 Pure)) => MonadCtx Pure (ConstrainSet () (Proxy StateT, (Proxy ReaderT, ()))) (t0 (t1 Pure)) Int)
--    --let x = (flip runReaderTX' (0::Int)) $ (flip runStateTX' (0::Int)) (MonadCtx $ (lift . lift . fromValue) (Value $ Pure (1::Int)) :: (MonadTrans t0, MonadTrans t1, Monad (t1 Pure)) => MonadCtx base (ConstrainSet req (Proxy StateT, (Proxy ReaderT, ()))) (t0 (t1 Pure)) Int)

--            --print $ (flip runStateTX (0::Int)) >>> (Value $ Pure 1)




--    --print $ addMeSingle' >>> (Value $ Pure 1)
--    --print $
--        --print $ addMeSingle' >>> (Value $ Pure 1)
--        --print $ addMeSingleS' >>> getX
--        --print $ addMeSingleS' >>> (Value $ Pure 1)

--    --print $ addMeSingleS' >>> (getX `bindEnv_` askX)

--    --print $ addMeSingleS' >>> askX
--    --print $ addMeSingleS' $ (MonadCtx $ liftValue (Value $ Pure (1::Int)))
--    print "end" 



----instance PolyApplicative Pure Pure Pure where
----    Pure f <<*>> Pure a = Pure $ f a

----instance PolyApplicative IO Pure IO where
----    f <<*>> Pure a = f <*> return a

----instance PolyApplicative Pure IO IO where
----    Pure f <<*>> a = a >>= (return . f)

----instance PolyApplicative IO IO IO where
----    f <<*>> a = f <*> a

-------

----instance PolyApplicative Pure (MonadCtx env set m) (MonadCtx env set m) <= Functor m where
----    (Pure f) <<*>> a = f <$> a

----instance PolyApplicative (MonadCtx env set m) Pure (MonadCtx env set m) <= (Functor m, Monad m) where
----    f <<*>> (Pure a) = f <*> pure a


----instance PolyApplicative IO (MonadCtx env set m) (MonadCtx envout set m) <= (MonadIO m, envout ~ EnvMerge env IO) where
----    mf <<*>> ma = MonadCtx $ do
----        f <- liftIO mf
----        a <- fromMonadCtx ma
----        return $ f a

----instance PolyApplicative (MonadCtx env set m) IO (MonadCtx envout set m) <= (MonadIO m, envout ~ EnvMerge env IO) where
----    mf <<*>> ma = MonadCtx $ do
----        f <- fromMonadCtx mf
----        a <- liftIO ma
----        return $ f a

----instance PolyApplicative (MonadCtx env1 set1 m1) (MonadCtx env2 set2 m2) (MonadCtx envout setout m1) <= (envout ~ EnvMerge env1 env2, setout ~ Union set1 set2, m1~m2, Monad m1) where
----    mf <<*>> ma = MonadCtx $ do
----        f <- fromMonadCtx mf
----        a <- fromMonadCtx ma
----        return $ f a
