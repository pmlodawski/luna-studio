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


module Luna.Target.HS.Control.Context.Pipe3 where

import Control.PolyMonad
import Control.Applicative
import Control.PolyApplicative
import Luna.Target.HS.Control.Context.Env
import Luna.Target.HS.Control.Context.Value
import Luna.Target.HS.Control.Context.Monad
import Luna.Target.HS.Control.Context.App
import Luna.Target.HS.Control.Flow.Lift
import Data.TypeLevel
import Control.Monad.IO.Class
import Control.PolyApplicative.App
import Luna.Target.HS.Utils.BaseMonads
import Data.Typeable
import Luna.Target.HS.Control.Context.Bind
import Control.Monad.Trans.Class (MonadTrans, lift)

import Control.Monad (join)

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

class Pipe a b c | a b -> c where
    pipe :: a -> b -> c


class Pipe2 m1 m2 where
    pipe2 :: (m1 a -> b) -> m2 a -> b


class Pipe2x m1 s1 m2 s2 where
    pipe2x :: (m1 (s1 a) -> b) -> m2 (s2 a) -> b

instance Pipe2x m1 s1 m2 s2 where
    pipe2x = undefined

data Y a = Y a

cons_Y = liftF1 Y

--tstx a = cons_Y `pipe2x` a

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Pipe2 (Req req (MonadCtx env1 set1 m1)) (MonadCtx env2 set2 m2) <= (env1~env2, m1~m2, set1~set2) where
    pipe2 f = f . Req

instance Pipe2 (Req req (MonadCtx env set m1)) (Value m2) <= (set~Insert req Empty, m1~t m2, MonadTrans t, Monad m2) where
    pipe2 f = f . matchReqMonadCtx . lift . fromValue

instance Pipe2 m1 (MonadCtx envout set2 m2) <= (m1 ~ MonadCtx envout set2 m2) where
    pipe2 = ($)

instance Pipe2 m1 (Value m2) <= (m1 ~ Value m2) where
    pipe2 = ($)

-------------------------
--instance Pipe (Value m1 a1 -> mb b) (Value m2 a2) out <= (out~mb b, a1~a2, m1~m2) where
--    pipe = ($)

instance Pipe (Req req (MonadCtx env1 set1 m1) a1 -> mb b) (MonadCtx env2 set2 m2 a2) out <= (out~mb b, env1~env2, m1~m2, a1~a2, set1~set2) where
    pipe f = f . Req

instance Pipe (Req req (MonadCtx env set m1) a1 -> mb b) (Value m2 a2) out <= (out~mb b, a1~a2, set~Insert req Empty, m1~t m2, MonadTrans t, Monad m2) where
    pipe f = f . matchReqMonadCtx . lift . fromValue

--instance Pipe (Value m1 a1 -> mb b) (MonadCtx env2 set2 m2 a2) out <= (out~mout b, m1~Pure, a1~a2, PolyMonad (MonadCtx env2 set2 m2) mb mout) where
--    pipe f a = a >>=~ fn where
--        fn = f . Value . Pure

instance Pipe (m1 a1 -> mb b) (MonadCtx envout set2 m2 a2) out <= (out~mb b, a1~a2, m1 ~ MonadCtx envout set2 m2) where
    pipe = ($)

instance Pipe (m1 a1 -> mb b) (Value m2 a2) out <= (out~mb b, a1~a2, m1 ~ Value m2) where
    pipe = ($)

--instance Pipe (m1 a1 -> b) a2 out <= (out~b, m1 a1~a2) where
--    pipe = ($)



--instance Pipe (a1 -> b) a2 out <= (out~b, a1~a2) where
--    pipe = ($)



--instance Pipe (m1 a1 -> mb b) (m2 a2) out <= (out~mb b, a1~a2, m1 ~ m2) where
--    pipe = ($)

--testx f = pipe f (return 5)




(>>>) = pipe

--liftEnv1x = app1 . Value . app1 . Pure

--addMe :: Int -> Int -> Int
addMe = (+)

addMeSingle :: Int -> Int
addMeSingle a = a + a

addMe' :: (PolyApplicative (Value Pure) m5 m1, PolyApplicative m1 m2 m3, Num b) => m5 b -> m2 b -> m3 b
addMe' = liftEnv2 addMe

addMeDummy a b c = a + b

addMeDummy' = liftEnv3 addMeDummy

addMe'' :: (PolyApplicative Pure ma out, PolyApplicative out mb out1, Num a) => Value ma a -> Value mb a -> Value out1 a 
addMe'' = liftEnv2 addMe

addMeSingle' a = liftEnv1 addMeSingle a

addMeSingle'' :: PolyApplicative (Value Pure) (Value m2) m3 => Value m2 Int -> m3 Int
addMeSingle'' = liftEnv1 addMeSingle

addMeSingle'2 (a :: Value base Int) = liftEnv1 addMeSingle a

--addMeSingle'2 a = Value $ addMeSingle' a
addMeSingleS' (a :: MonadCtx env set (StateT Int mb) Int ) = addMeSingle' $ fmap fst $ runStateTX a (0::Int)

--addMeSingleS'2 (a :: MonadCtx env set (StateT Int mb) Int ) = addMeSingle'2 >>> (fmap fst $ runStateTX a (0::Int))

--instance MonadIO Pure where
--    liftIO = undefined

        --runStateTX
        --  :: MatchMonadCloseProto
        --       (IsEmpty (Remove (Proxy StateT) set))
        --       (MonadCtx env (Remove (Proxy StateT) set) mb)
        --       t =>
        --     MonadCtx env set (StateT a mb) a1 -> a -> t (a1, a)
        --- trzeba zmienic "set" z "MonadCtx env set [...]" na cos co mowi co MUSI byc spelnione!
        --- aby dzialalo: (flip runStateTX (0::Int)) >>> (Value $ Pure 1)

--matchReqMonadCtx :: m val -> MonadCtx base (ConstrainSet set set) m val
--matchReqMonadCtx = MonadCtx

matchReqMonadCtx :: m val -> Req req (MonadCtx base (Insert req Empty) m) val
matchReqMonadCtx = Req . MonadCtx


main = do
    let x = return 5 :: Value Pure Int

    
    print $ addMeSingle'' >>> (Value $ Pure 1)
    print $ addMeSingle' >>> (Value $ Pure 1)

    print $ addMe'' >>> (Value $ Pure 1) >>> (Value $ Pure 4)
    print $ addMe' >>> (Value $ Pure 1) >>> (Value $ Pure 4)
    ---
    print $ (flip runStateTX' (0::Int)) >>> getX -- (Value $ Pure 1)
    print $ (flip runStateTX' (0::Int)) >>> (Value $ Pure 1)

    print $ (flip runStateTX' (0::Int)) >>> ((flip runStateTX' (0::Int)) >>> (Value $ Pure 1) )
    --print $ (flip runStateTX' (0::Int)) (matchReqMonadCtx $ (lift . fromValue) (Value $ Pure 1))

    ----print $ (flip runStateTX (0::Int)) $ addMeSingle'' >>> getX
    
    --print $ (flip runStateTX (0::Int)) $ addMeSingle' >>> getX


    --print $ (flip runStateTX (0::Int)) $ addMe' >>> getX >>> getX
    
    --print $ flip runReaderTX (0::Int) $ flip runStateTX (0::Int) $ addMe' >>> getX >>> askX
  
    putStrLn "---"

    --print $ tuplePipe addMeSingle'' (Value $ Pure 1, ())
    --print $ (flip runStateTX (0::Int)) $ tuplePipe addMe' (getX, (getX, ()))

    putStrLn "end" 


