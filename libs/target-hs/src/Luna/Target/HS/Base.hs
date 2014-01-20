---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


--{-# LANGUAGE OverlappingInstances #-}

module Luna.Target.HS.Base where

import Data.Typeable
import Control.Applicative
import GHC.Generics        (Generic)
import GHC.TypeLits        (Symbol)

import Luna.Target.HS.Data

------------------------------------------------------------------------
-- Bind
------------------------------------------------------------------------


class Bind m1 m2 m3 | m1 m2 -> m3 where
    bind :: m1 a -> (Pure a -> m2 b) -> m3 b

instance Bind Pure Pure Pure where
    bind a b = b a

instance Bind IO Pure IO where
    bind a b = do
        vala <- a
        let Pure valb = b (Pure vala)
        return valb

instance Bind Pure IO IO where
    bind a b = b a

--instance (a ~ Pure) => Bind Pure a where
--    bind a b = b a

instance Bind IO IO IO where
    bind a b = do
        x <- a
        b (Pure x)


bind_ a b = bind a (\_ -> b)


--class Bind m1 m2 m3 | m1 m2 -> m3 where
--    bind :: m1 a -> (Pure a -> m2 b) -> m3 b


--instance Bind Pure Pure Pure where
--    bind a b = b a

--instance Bind IO Pure IO where
--    bind a b = do
--        vala <- a
--        let Pure valb = b (Pure vala)
--        return valb

--instance Bind Pure IO IO where
--    bind a b = b a

--instance Bind IO IO IO where
--    bind a b = do
--        x <- a
--        b (Pure x)

--instance (Pure ~ a, Pure ~ b) => Bind Pure a b where
--    bind a b = b a

--instance (IO ~ io) => Bind IO io IO where
--    bind a b = do
--        x <- a
--        b (Pure x)


------------------------------------------------------------------------
-- BindErr
------------------------------------------------------------------------


class BindErr m1 m2 m3 | m1 m2 -> m3 where
    bindErr :: m1 a -> (Safe a -> m2 b) -> m3 b

instance BindErr Safe Safe Safe where
    bindErr a b = b a

instance BindErr (Either e) Safe (Either e) where
    bindErr a b = do
        vala <- a
        let Safe valb = b (Safe vala)
        return valb

instance BindErr Safe (Either e) (Either e) where
    bindErr a b = b a

instance BindErr (Either e) (Either e) (Either e) where
    bindErr a b = do
        x <- a
        b (Safe x)



------------------------------------------------------------------------
-- Get IO
------------------------------------------------------------------------

class GetIO m where
    getIO :: m a -> IO a

instance GetIO IO where
    getIO = id

instance GetIO Pure where
    getIO (Pure val) = return val



------------------------------------------------------------------------
-- EvalEnvProto
------------------------------------------------------------------------

class EvalEnvProto a m b | a -> b where
    evalProto :: a -> m b

instance (EvalEnvProto a m b, Functor m) => EvalEnvProto (Pure a) m (Pure b) where
    evalProto (Pure a) = Pure <$> evalProto a 

instance (Monad m, EvalEnvProto a m b) => EvalEnvProto [a] m [b] where
    evalProto a = sequence $ map evalProto a

instance (Functor m, EvalEnvProto a m b) => EvalEnvProto (Safe a) m (Safe b) where
    evalProto (Safe a) = Safe <$> evalProto a

instance (b ~ Pure a) => EvalEnvProto (IO a) IO b where
    evalProto a = Pure <$> a

instance (EvalEnvProto a m a2, EvalEnvProto b m b2, Functor m) => EvalEnvProto (Either a b) m (Either a2 b2) where
    evalProto e = case e of 
        Left  val -> Left  <$> evalProto val
        Right val -> Right <$> evalProto val

instance (Monad m) => EvalEnvProto Int m Int where
    evalProto = return

instance (Monad m) => EvalEnvProto Bool m Bool where
    evalProto = return

instance (Monad m) => EvalEnvProto Char m Char where
    evalProto = return


eval :: EvalEnvProto a IO (Pure b) => a -> IO b
eval a = do Pure (ioval) <- evalProto a
            return ioval

--class EvalEnv a m b| a -> b where
--    eval :: a -> m b

--instance (EvalEnvProto a IO b) => EvalEnv (Pure a) IO b where
--    eval a = do Pure (ioval) <- evalProto a
--                return ioval

--instance (EvalEnvProto a IO b) => EvalEnv (IO a) IO a where
--    eval a = do Pure (ioval) <- evalProto a
--                return ioval

--instance (EvalEnvProto a m b, Functor m) => EvalEnv (Pure a) m (Pure b) where
--    eval a = evalProto a






data MyError = MyError deriving (Show, Typeable)

instance (Monad m) => EvalEnvProto MyError m MyError where
    evalProto = return

-- IMPLEMENT ME
instance (Monad m) => EvalEnvProto (a,b) m MyError where
    evalProto = undefined

--class EvalEnvProto a b | a -> b where
--    EvalEnvProto :: a -> IO b

--instance EvalEnvProto a b => EvalEnvProto (Pure a) b where
--    EvalEnvProto (Pure a) = EvalEnvProto a 

--instance EvalEnvProto a b => EvalEnvProto [a] [b] where
--    EvalEnvProto a = sequence $ map EvalEnvProto a

--instance EvalEnvProto a b => EvalEnvProto (Safe a) b where
--    EvalEnvProto (Safe a) = Safe <$> EvalEnvProto a

--instance EvalEnvProto (IO a) a where
--    EvalEnvProto = id

--instance EvalEnvProto Char Char where
--    EvalEnvProto = return

--instance (EvalEnvProto a a2, EvalEnvProto b b2) => EvalEnvProto (Either a b) (Either a2 b2) where
--    EvalEnvProto e = case e of 
--        Left  val -> Left  <$> EvalEnvProto val
--        Right val -> Right <$> EvalEnvProto val

--print' :: (EvalEnvProto a b, Show b, Show a) => a -> IO ()
--print' s = print =<< EvalEnvProto s

------------------------------------------------------------------------
-- LiftEnv
------------------------------------------------------------------------

class LiftEnv m1 m2 m3 | m1 m2 -> m3 where
    liftenv :: m1 (a -> b) -> m2 a -> m3 b


instance LiftEnv Pure Pure Pure where
     liftenv (Pure f) (Pure a) = Pure $ f a

instance LiftEnv IO Pure IO where
     liftenv f (Pure a) = f <*> pure a

instance LiftEnv Pure IO IO where
     liftenv (Pure f) a = f <$> a

instance LiftEnv IO IO IO where
     liftenv f a = f <*> a


liftenv2 f a b   = liftenv (liftenv f a) b
liftenv3 f a b c = liftenv (liftenv2 f a b) c


liftf  = liftenv  . Pure . liftErr  . Safe
liftf2 = liftenv2 . Pure . liftErr2 . Safe
liftf3 = liftenv3 . Pure . liftErr3 . Safe


------------------------------------------------------------------------
-- FlattenEnv
------------------------------------------------------------------------

class FlattenEnv m1 m2 m3 | m1 m2 -> m3 where
    flattenEnv :: m1 (m2 a) -> m3 a

instance FlattenEnv Pure Pure Pure where
    flattenEnv (Pure a) = a

instance FlattenEnv Pure IO IO where
    flattenEnv (Pure a) = a

instance FlattenEnv IO Pure IO where
    flattenEnv a = do Pure val <- a
                      return val

instance FlattenEnv IO IO IO where
    flattenEnv a = do val <- a
                      val

--instance a ~ Pure => FlattenEnv Pure a a where
--    flattenEnv (Pure a) = a



------------------------------------------------------------------------
-- FlattenErr
------------------------------------------------------------------------

class FlattenErr m1 m2 m3 | m1 m2 -> m3 where
    flattenErr :: m1 (m2 a) -> m3 a

instance FlattenErr Safe Safe Safe where
    flattenErr (Safe a) = a

instance FlattenErr Safe (Either e) (Either e) where
    flattenErr (Safe a) = a

instance FlattenErr (Either e) Safe (Either e) where
    flattenErr a = case a of
        Left e           -> Left e
        Right (Safe val) -> Right val

instance FlattenErr (Either e) (Either e) (Either e) where
    flattenErr a = case a of
        Left  e -> Left e
        Right b -> case b of
            Left e    -> Left e
            Right val -> Right val


--type family FlattenErrResult a b where
--    FlattenErrResult Safe       Safe       = Safe
--    FlattenErrResult Safe       (Either e) = (Either e)
--    FlattenErrResult (Either e) Safe       = (Either e)
--    FlattenErrResult (Either e) (Either e) = (Either e)


------------------------------------------------------------------------
-- FlipCtx -- Full implementation, some flips are not allowed like IO(Safe(..)) <-> Safe(IO(..))
------------------------------------------------------------------------

class FlipCtx m1 m2 where
    flipCtx :: m1 (m2 a) -> m2 (m1 a)

instance FlipCtx Safe Pure where
    flipCtx (Safe(Pure a)) = Pure(Safe a)

instance FlipCtx (Either e) Pure where
    flipCtx a = case a of
        Left e           -> Pure (Left e)
        Right (Pure val) -> Pure (Right val)

instance FlipCtx Safe IO where
    flipCtx (Safe a) = Safe <$> a

instance FlipCtx (Either e) IO where
    flipCtx a = case a of
        Left e    -> return (Left e)
        Right val -> Right <$> val


------------------------------------------------------------------------
-- LiftErr
------------------------------------------------------------------------


class LiftErr m1 m2 m3 | m1 m2 -> m3 where
    liftErr :: m1 (a -> b) -> m2 a -> m3 b

instance LiftErr Safe Safe Safe where
    liftErr (Safe f) (Safe a) = Safe (f a)

instance LiftErr Safe (Either e) (Either e) where
    liftErr (Safe f) b = f <$> b

instance LiftErr (Either e) (Either e) (Either e) where
    liftErr f a = f <*> a

instance LiftErr (Either e) Safe (Either e) where
    liftErr f (Safe a) = f <*> pure a 

liftErr2 f a b   = liftErr (liftErr f a) b
liftErr3 f a b c = liftErr (liftErr2 f a b) c



------------------------------------------------------------------------
-- Members
------------------------------------------------------------------------



--class Member (name :: Symbol) cls func | name cls -> func where 
--    member :: proxy name -> cls -> func

--class Member (name :: Symbol) cls func | name cls -> func where 
--    member :: proxy name -> cls -> func


class MemberProto (name :: Symbol) cls func | name cls -> func where 
    memberProto :: proxy name -> cls -> func


class Call ptr args result | ptr args -> result where
    callProto :: ptr -> args -> result



--class Call0 a b | a -> b where
--    call0 :: a -> b

--class Call1 a b where
--    call1 :: a -> b

--class Call2 a b | a -> b where
--    call2 :: a -> b

--class Call3 a b | a -> b where
--    call3 :: a -> b

class KWSet (name :: Symbol) val m where
    kwset :: proxy name -> val -> m f -> m f


------------------------------------------------------------------------
-- AppH
------------------------------------------------------------------------

newtype AppH func arg = AppH (func, arg) deriving Show

instance (Call f (a, b) c) => Call (AppH f a) b c where
    callProto (AppH (func, arg)) args = callProto func (arg, args)

appH :: a -> b -> AppH a b
appH a b = AppH (a,b)

fcurry :: (Functor m, Functor s) => arg -> m(s func) -> m(s(AppH func arg))
fcurry arg = (fmap.fmap) (flip appH arg)