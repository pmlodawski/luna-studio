---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}


{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}


!{-# LANGUAGE RightSideContexts #-}

{-# LANGUAGE DysfunctionalDependencies #-}


module Luna.Target.HS.Utils.BaseMonads where

import Luna.Target.HS.Control.Error
import Luna.Target.HS.Control.Flow
import Control.Monad.Trans
import Control.Monad.Morph
import Data.Typeable (Typeable)
import Luna.Target.HS.Control.Context
import Data.TypeLevel
import Data.Typeable
import Control.Applicative
import Control.Monad (ap)

import Control.PolyMonad

--print' = liftIO . print


instance MonadIO (t m) <= (MonadIO m, MonadTrans t, Monad (t m)) where
    liftIO = lift . liftIO

--------------------------------------------------------------------------------
-- ReaderT monad
--------------------------------------------------------------------------------


newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a } deriving (Typeable)

class Monad m => MonadReader r m | m -> r where
    {-# MINIMAL (ask | reader), local #-}
    ask   :: m r
    ask = reader id

    local :: (r -> r) -> m a -> m a

    reader :: (r -> a) -> m a
    reader f = do
      r <- ask
      return (f r)

withReaderT f m = ReaderT $ runReaderT m . f

instance MonadTrans (ReaderT r) where
    lift m = ReaderT (const m)

instance (Monad m) => Monad (ReaderT r m) where
    return   = lift . return
    m >>= k  = ReaderT $ \ r -> do
        a <- runReaderT m r
        runReaderT (k a) r
    fail msg = lift (fail msg)

instance Monad m => MonadReader r (ReaderT r m) where
    ask = ReaderT return
    local = withReaderT
    reader f = ReaderT (return . f)

instance MFunctor (ReaderT r) where
    hoist nat m = ReaderT (\i -> nat (runReaderT m i))

instance (Monad m) => Functor (ReaderT r m) where
    fmap f m = ReaderT $ \r -> do
        a <- runReaderT m r
        return (f a)


--------------------------------------------------------------------------------
-- StateT monad
--------------------------------------------------------------------------------

-- data Vector a = Vector { x :: a 
--                        , y :: a
--                        , z :: a
--                        , t :: a -> a
--                        } deriving Show
 
-- class Vector a:
--     x,y,z :: a
--     t     :: a -> a
 
 
-- ###
 
-- monad State s a:
--     val :: s -> ctx (a, s)
 
--     def wrap val:
--         State s: ctx.wrap (val, s)
 
--     def bind f:
--         State s:
--             (val2, subs) = unwrap (val s)
--             (f val2).val subs
 
-- def get     : State s: (s, s)
-- def put val : State _: ((), val)
-- def run st ::(a in State s) init::s : 
--     s.val(init).map(@.fst)
 
-- ### <=>
 
-- context State a:
--     val :: a
 
--     def get: val
 
--     def put n:
--         self.val = n
 
 
-- konwersja w obie strony: monad <-> wartosc
-- moze gadtsy?
-- dowolne metody - zachowanie jak w klasach
 
-- >>= :: m a -> (a -> m b) -> m b

 --class Maybe a:
 --   Just a
 --   Nothing

 --   def wrap a: Just (ctx a)

 --   def bind f:
 --       case self:
 --           Just a : f a
 --           Nothing: Nothing




 
---  :: Value Pure (Safe (Maybe (Value Pure (Safe Int))))
---  -> Value Pure (Safe (MonadCtx Pure (Maybe,()) m (Value Pure (Safe Int))))
---
---  :: Value Pure (Safe (Maybe (Value Pure (Safe Int))))
---  -> MonadCtx Pure (Maybe,()) m (Safe (Value Pure (Safe Int)))
 

---  -> MonadCtx Pure (Maybe,()) m (Safe Int)

 
-- Value Pure (Safe (Maybe Int)) -> Value Pure (Safe (MonadCtx Pure (Maybe,()) m Int))
 
-- Value Pure (Safe (Maybe Int)) -> (MonadCtx Pure (Maybe,()) m (Safe Int))


--MonadCtx Pure (Maybe,()) m (Safe Int)

--put (x :: Value IO (Safe Int)) :: MonadCtx IO (Maybe,()) m (Safe Int)

--runStateT ... :: Value Pure (Safe    (Value IO(Safe Int) , Value IO(Safe Int))   ) 


--put (x :: Value IO (Safe Int)) :: MonadCtx IO (Maybe,()) m (Value IO (Safe Int))


--Pure (Safe val)

--MonadCtx Safe 

--runMonadCtx -> Pure Safe val

newtype StateT2 v m s a = StateT2 { runStateT2 :: v -> m (s (a,v)) } deriving (Typeable)
newtype StateT3 v m s a = StateT3 { runStateT3 :: v -> m s (a,v) } deriving (Typeable)

data E1 = E1 deriving Show

data Safe2 a = Safe2 a deriving Show
data Unsafe2 a = Unsafe2 a deriving Show


--liftIO :: IO a -> m a

--instance MonadIO Safe2 where
--    liftIO a 

class Monad2 m where
    return2 :: Monad safety => a -> m safety a
    bind2   :: m safety1 a -> (a -> m safety2 b) -> m safety2 b

class IsError a where
    isError :: a -> Prelude.Bool

--instance Monad m => Monad2 (StateT2 s m) where
--    return2 a = StateT2 $ \s -> return $ return (a,s)
--    m `bind2` k  = StateT2 $ \s -> do
--        s1 <- runStateT2 m s
--        if isError s1 then return s1
--                      else return s1
        --do
        --    ~(a, s') <- s1
        --    runStateT2 (k a) s'
    --fail str = failErr


------------------------

newtype MonadCtx2 (base :: * -> *) set m s val = MonadCtx2 (m s val) deriving (Show, Typeable)



type family MatchEnv e1 e2 where
    MatchEnv IOS IOS = IOS
    MatchEnv IOS IOS = IOS


--Pure (Safe Int)

--MonadCtx Pure (Proxy StateT3, ()) m a

--PureS Safe Int


--StateT3 v m s a

--MonadT m s a

class Monad3R m s where
    return3 :: (forall x. x -> s x) -> a -> m s a

wrap3 = return3 Safe


class Monad3 m s1 s2 s3 | s1 s2 -> s3 where
    bind3 :: m s1 a -> (a -> m s2 b) -> m s3 b

class Monad4 m s1 s2 where
    bind4 :: m s1 a -> (a -> m s2 b) -> m (MatchSafety s1 s2) b


class PolyMonad4 m1 m2 s1 s2 where
    polyBind4 ::  PolyMonad (m1 s1) (m2 s2) ((MatchEnv m1 m2) (MatchSafety s1 s2)) => m1 s1 a -> (a -> m2 s2 b) -> (MatchEnv m1 m2) (MatchSafety s1 s2) b

--instance Monad m => Monad3 (StateT2 s m) Safe s2 s2 where
--    bind3 m f = StateT2 $ \ s -> do
--        Safe (a, s') <- runStateT2 m s
--        runStateT2 (f a) s'

newtype IOS s a = IOS {fromIOS :: IO (s a)}

newtype PureS s a = PureS {fromPureS :: Pure (s a)}


instance PolyMonad4 IOS IOS Safe s where
    polyBind4 m f = IOS $ do
        Safe a <- fromIOS m
        fromIOS $ f a


instance Monad3R IOS m <= Monad m where
    return3 fs a = IOS . return $ fs a


instance Monad4 IOS Safe s where
    bind4 m f = IOS $ do
        Safe a <- fromIOS m
        fromIOS $ f a


instance Monad4 IOS (UnsafeBase base err) Safe where
    bind4 m f = IOS $ do
        sa <- fromIOS m
        case sa of
            UnsafeValue a -> fmap (UnsafeValue . fromSafe) $ fromIOS $ f a
            Error e       -> return $ Error e
        --fromIOS $ f a


--instance Monad4 PureS Safe s where
--    bind4 m f = PureS $ do
--        Safe a <- fromPureS m
--        fromPureS $ f a


--instance Monad4 PureS (UnsafeBase base err) Safe where
--    bind4 m f = fromPureS m



--s1 a -> (a -> s2 b) -> s3 b

--s1 a -> (a -> m (s2 b)) -> m (s3 b)

--instance Monad3 (StateT3 s m) Safe s2 s3 <= Monad3 m Safe s2 s3 where
--    bind3 m f = StateT3 $ \ s ->
--        runStateT3 m s `bind3` (\(a, s')->
--        runStateT3 (f a) s')

--newtype StateT3 v m s a = StateT3 { runStateT3 :: v -> m s (a,v) } deriving (Typeable)


instance Monad4 (StateT3 s m) s1 s2 <= Monad4 m s1 s2 where
    bind4 m f = StateT3 $ \ s ->
        runStateT3 m s `bind4` (\(a, s')->
        runStateT3 (f a) s')




instance Monad3R (StateT3 s m) safety <= (Monad3R m safety) where
    return3 fs a = StateT3 $ \s -> return3 fs (a, s)


class MonadState2 v m | m -> v where
    put3' :: v -> m Safe v


instance MonadState2 v (StateT3 v m) <= Monad3R m Safe where
    put3' s = StateT3 $ \_ -> wrap3 (s,s)



put3 :: Monad3R m Safe => v -> StateT3 v m Safe v
put3 s = StateT3 $ \_ -> wrap3 (s,s)

--raiseMe :: e -> m (UnsafeBase Safe e) () <= Monad3R m (UnsafeBase Safe e)
--raiseMe e = return3 ()

raiseMe :: e -> m (UnsafeBase Safe e) () <= Monad3R m (UnsafeBase Safe e)
raiseMe e = return3 (\_ -> Error e) ()

tst1 = put3 (0::Int) `bind4` (\_ -> put3 (0::Int))
tst1' = put3' (0::Int) `bind4` (\_ -> put3' (0::Int))

tst2 = put3 (0::Int) `bind4` (\_ -> raiseMe E1)
tst2' = put3' (0::Int) `bind4` (\_ -> raiseMe E1)

tst3 = raiseMe E1 `bind4` (\_ -> put3 (0::Int))
tst3' = raiseMe E1 `bind4` (\_ -> put3' (0::Int))

put3X = MonadCtx2 . put3'


tstB1 = val 0 `bindEnv_` val 1

--tstB2 = put3X (val 0) `bindEnv_` val 1



--instance MonadState2 v (t v m) where
--    put3' s = lift $ put3' s


--instance Monad3 (StateT2 s m) (UnsafeBase base err) s2 s3 where
--    bind3

--tst = do
--    liftIO $ print "hello" :: MonadIO m => Val m Safe2
--    raise E1               :: MonadIO m => Val m (Unsafe2 E1)
--    liftIO $ print "world" :: MonadIO m => Val m Safe2


--class Monad3 m safety1 safety2 safety3 | safety1 safety2 -> safety3 where
--    bind3   :: m safety1 a -> (a -> m safety2 b) -> m safety3 b

------------------------

instance Monad Safe2 where
    return = Safe2
    Safe2 a >>= f = f a

class Error a m where
    error' :: a -> m x

--instance Error E1 Safe2 where
--    error' _ = 

instance (Monad m, Monad safety) => Monad (StateT2 s m safety) where
    return a = StateT2 $ \s -> return $ return (a, s)
    --m >>= k  = StateT2 $ \s -> do
    --    ~(a, s') <- runStateT2 m s
    --    runStateT2 (k a) s'
    --fail str = failErr

failErr = StateT2 $ \_ -> return $ error' E1

    --get2 = StateT2 $ \s -> return (s, s)
foo2 :: (Monad m) => s -> StateT2 s m Safe2 s
foo2 s = StateT2 $ \_ -> return $ return (s, s)

data Val t m a = Val (t (m a)) deriving Show

instance x~Safe2 => MonadIO (Val IO x) where
    liftIO ioa = Val $ (fmap Safe2) ioa

instance Monad (Val t m) where

    
    --instance (Monad t, Monad m) => Monad (Val t m) where
    --    return = Val . return . return

--tst = do
--    foo2 (0::Int)
    --failErr

    --fail str = StateT $ \_ -> fail str

--toIO :: m~IO => m a -> m a; toIO = id

matchT :: a -> a -> a
matchT = const

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) } deriving (Typeable)

instance (Monad m) => Monad (StateT s m) where
    return a = state $ \s -> (a, s)
    m >>= k  = StateT $ \s -> do
        ~(a, s') <- runStateT m s
        runStateT (k a) s'
    fail str = StateT $ \_ -> fail str

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        a <- m
        return (a, s)


instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT $ \ s ->
        fmap (\ ~(a, s') -> (f a, s')) $ runStateT m s


instance (MonadReader r m) => MonadReader r (StateT s m) where
    ask       = lift ask
    local f m = StateT $ \s -> local f (runStateT m s)


class (Monad m) => MonadState s m | m -> s where
    get :: m s
    put :: s -> m (Safe (Value Pure (Safe ())))

instance MonadState s (StateT s m) <= Monad m where
    get   = StateT $ \s -> return (s, s)
    put s = StateT $ \_ -> return (Safe $ val (), s)

instance MonadState s (ReaderT s m) <= MonadState s m where
    get   = lift $ get
    put s = lift $ put s

instance MonadState s (t s m) <= (MonadTrans (t s), MonadState s m, Monad (t s m)) where
    get   = lift $ get
    put s = lift $ put s

instance MFunctor (StateT s) where
    hoist nat m = StateT (\s -> nat (runStateT m s))

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure = return
    (<*>) = ap

state f = StateT (return . f)


----------------------------------------------------------------------

getX2 :: MonadCtx Pure (Insert (Proxy StateT) Empty) m s <= MonadState s m
getX2 = MonadCtx get

getX :: MonadCtx Pure (Insert (Proxy StateT) Empty) m s <= MonadState s m
getX = MonadCtx get

putX :: s -> MonadCtx Pure (Insert (Proxy StateT) Empty) m (Safe (Value Pure (Safe ()))) <= MonadState s m
putX = MonadCtx . put

askX :: MonadCtx Pure (Insert (Proxy ReaderT) Empty) m s <= MonadReader s m
askX = MonadCtx ask

--getX' :: MonadCtx Pure (ConstrainSet () (Insert (Proxy StateT) Empty)) m s <= MonadState s m
--getX' = MonadCtx get

runStateT' a s = fmap Safe $ runStateT a s

runStateTX  = liftMonadRunner1 (Proxy :: Proxy StateT)  runStateT  . appMonadCtx
runReaderTX = liftMonadRunner1 (Proxy :: Proxy ReaderT) runReaderT . appMonadCtx

--runStateTX''  = liftMonadRunner1'' (Proxy :: Proxy StateT)  runStateT
--runReaderTX'' = liftMonadRunner1'' (Proxy :: Proxy ReaderT) runReaderT


--liftMonadRunner1' :: MatchMonadCloseProto (IsEmpty (Remove mptr set)) (MonadCtx env (Remove mptr set) mb) t => mptr -> (ma a1 -> a -> mb b) -> MonadCtx env set ma a1 -> a -> t b
--liftMonadRunner1' (mf :: (t x (ma :: * -> *) a1 -> a -> mb b)) = liftMonadRunner1 (Proxy :: Proxy t) mf

putX' = putX . Safe
getX' = fmap fromSafe getX

--xxx = do
--    x <- getX
--    putX x