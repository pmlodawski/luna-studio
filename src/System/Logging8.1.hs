{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude   as P hiding (log, lookup)
--import Data.String.Class (ToString(toString))
import qualified Control.Monad.State as State
import           Control.Monad.State (StateT, runStateT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Applicative hiding (empty)
import Data.Generics (Generic)
import Data.Monoid
import Control.Monad.Identity (Identity, runIdentity)
import Data.Sequence (Seq, (|>))
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy (Text)
import Control.Lens hiding ((|>), children, LevelData, Level)
import Control.Monad (when)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (toList)

import Data.Time (getCurrentTime)
import Data.Typeable
import System.IO (stdout)
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>), empty)
import Control.Concurrent (threadDelay)
import           Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import           Control.Concurrent.Chan.Unagi (readChan, writeChan, newChan, InChan, OutChan)
import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, MVar)
import           Control.Exception       (finally, onException, throwIO, catch, SomeException)

import System.Log.Level
import System.Log.Log (Log(Log), fromLog)
import System.Log.Data
import System.Log.Formatter


type family Tuple2RTuple a where
    Tuple2RTuple ()                               = ()
    Tuple2RTuple (t1,t2)                          = (t1,(t2,()))
    Tuple2RTuple (t1,t2,t3)                       = (t1,(t2,(t3,())))
    Tuple2RTuple (t1,t2,t3,t4)                    = (t1,(t2,(t3,(t4,()))))
    Tuple2RTuple (t1,t2,t3,t4,t5)                 = (t1,(t2,(t3,(t4,(t5,())))))
    Tuple2RTuple (t1,t2,t3,t4,t5,t6)              = (t1,(t2,(t3,(t4,(t5,(t6,()))))))
    Tuple2RTuple (t1,t2,t3,t4,t5,t6,t7)           = (t1,(t2,(t3,(t4,(t5,(t6,(t7,())))))))
    Tuple2RTuple (t1,t2,t3,t4,t5,t6,t7,t8)        = (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,()))))))))
    Tuple2RTuple (t1,t2,t3,t4,t5,t6,t7,t8,t9)     = (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,())))))))))
    Tuple2RTuple (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) = (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,()))))))))))
    Tuple2RTuple t                                = (t,())

type family RTuple2Tuple a where
    RTuple2Tuple ()                                                    = ()
    RTuple2Tuple (t1,(t2,()))                                          = (t1,t2)
    RTuple2Tuple (t1,(t2,(t3,())))                                     = (t1,t2,t3)
    RTuple2Tuple (t1,(t2,(t3,(t4,()))))                                = (t1,t2,t3,t4)
    RTuple2Tuple (t1,(t2,(t3,(t4,(t5,())))))                           = (t1,t2,t3,t4,t5)
    RTuple2Tuple (t1,(t2,(t3,(t4,(t5,(t6,()))))))                      = (t1,t2,t3,t4,t5,t6)
    RTuple2Tuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,())))))))                 = (t1,t2,t3,t4,t5,t6,t7)
    RTuple2Tuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,()))))))))            = (t1,t2,t3,t4,t5,t6,t7,t8)
    RTuple2Tuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,())))))))))       = (t1,t2,t3,t4,t5,t6,t7,t8,t9)
    RTuple2Tuple (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,())))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)
    RTuple2Tuple (t,())                                                = t



type family Insert t set where
  Insert t ()    = (t,())
  Insert t (t,x) = (t,x)
  Insert t (a,x) = (a,Insert t x)


type family MapRTuple (f :: * -> *) tup where
    MapRTuple f () = ()
    MapRTuple f (a,as) = (f a, MapRTuple f as)

class MapRTuple2 f tup tup' | f tup -> tup'
    where mapRTuple :: f -> tup -> tup'

instance MapRTuple2 f () () where
    mapRTuple _ = id

instance MapRTuple2 (a -> b) as bs => MapRTuple2 (a -> b) (a,as) (b,bs) where
    mapRTuple f (a,as) = (f a, mapRTuple f as)





--data DataProvider base d = 

-- TODO: move to System.Log.Log
type family LogFormat (m :: * -> *) where
    LogFormat (BaseLoggerT l m) = l
    LogFormat (a m)         = LogFormat m 



----------------------------------------------------------------------
-- BaseLoggerT
----------------------------------------------------------------------

newtype BaseLoggerT l m a = BaseLoggerT { runRawBaseLoggerT :: m a } deriving (Monad, MonadIO, Applicative, Functor) 

instance MonadTrans (BaseLoggerT l) where
    lift = BaseLoggerT

instance Monad m => LogConstructor d (BaseLoggerT l m) where
    mkLog _ = return ()

runBaseLoggerT :: (Functor m, Monad m) => l -> BaseLoggerT (MapRTuple Data (Tuple2RTuple l)) m a -> m a
runBaseLoggerT _ = runRawBaseLoggerT




----------------------------------------------------------------------
-- Formatters
----------------------------------------------------------------------



--class FormatterBuilder a b c where
--    buildFormatter :: a -> b -> c

--instance FormatterBuilder String String (Formatter a) where
--    buildFormatter a b = Formatter (const a) `buildFormatter` Formatter (const b)

--instance (a~b, a~c) => FormatterBuilder (Formatter a) (Formatter b) (Formatter c) where
--    buildFormatter (Formatter f) (Formatter g) = Formatter (\s -> f s <> g s)

--instance Lookup a l => FormatterBuilder String a b where
--    buildFormatter s a = Formatter (const s) `buildFormatter` Formatter (show . readData a)



--readData' :: (Lookup a r) => a -> Log l -> DataOf a
--readData' a = readData a



----------------------------------------------------------------------
-- MonadLogger
----------------------------------------------------------------------

class (Monad m, Applicative m) => MonadLogger m where
    appendLog :: Log (LogFormat m) -> m ()


class MonadLoggerHandler h m | m -> h where
    addHandler :: h -> m ()

defAddHandler = lift . addHandler

----------------------------------------------------------------------
-- LogBuilder
----------------------------------------------------------------------


class LogBuilder a m b where
    buildLog :: RecordBuilder a -> m (Log b)


instance (LogBuilder xs m ys, Functor m) => LogBuilder (Data x,xs) m (Data x,ys) where
    buildLog b = (fmap.fmap) (x,) $ buildLog $ RecordBuilder xs where
        (x,xs) = fromRecordBuilder b


instance (LogBuilder (Data x,xs) m ys, LogBuilder xs m (Data y,()), Monad m) => LogBuilder (Data x,xs) m (Data y,ys) where
    buildLog b = do
        let (x,xs) = fromRecordBuilder b
        Log ys     <- buildLog b
        Log (y,()) <- buildLog $ RecordBuilder xs
        return $ Log (y, ys)
        

instance Monad m => LogBuilder a m () where
    buildLog _ = return $ Log ()

instance (Functor m, Applicative m, DataGetter y m, LogBuilder () m ys) => LogBuilder () m (Data y,ys) where
    buildLog b = fmap Log $ (,) <$> getData <*> (fromLog <$> buildLog b)


type LogBuilder' a m = LogBuilder a m (LogFormat m)




buildLog' :: (Monad m, Applicative m, LogBuilder' a m) => RecordBuilder a -> m (Log (LogFormat m))
buildLog' = buildLog

class LogConstructor d m where
    mkLog :: RecordBuilder d -> m ()

        --instance (LogBuilder' d m) => LogConstructor d m where
        --    mkLog d = do
        --        l <- buildLog' d
        --        appendLog l


defMkLog d = do
    l <- buildLog' d
    appendLog l

----------------------------------------------------------------------
-- Handler & HandlerWrapper
----------------------------------------------------------------------


-- !!! dorobic formattery i filtracje do handlerow!
mkHandler2 :: String -> (Log l -> m ()) -> Handler2 m l
mkHandler2 name f = Handler2 name f [] Nothing

data Handler2 m l = Handler2 { _name2     :: String
                             , _action2   :: Log l -> m ()
                             , _children2 :: [Handler2 m l]
                             , _level2    :: Maybe Int
                             }

makeLenses ''Handler2

topHandler2 = mkHandler2 "TopHandler" (\_ -> return ())

instance Show (Handler2 m l) where
    show (Handler2 n _ _ pr) = "Handler " <> n <> " " <> show pr


-- === Handlers ===

--class LogHandler h m msg where
--    handle :: Handler msg m h -> Log msg -> m (Handler msg m h)

printHandler2 = mkHandler2 "PrintHandler" $ handle2

handle2 l = do
        liftIO $ putDoc $ runFormatter foo l
        liftIO $ putStrLn ""


addChildHandler2 h ph = ph & children2 %~ (h:)


-- === Loggers ===

newtype HandlerLogger m a = HandlerLogger { fromHandlerLogger :: StateT (Handler2 (HandlerLogger m) (LogFormat m)) m a } deriving (Monad, MonadIO, Applicative, Functor)

runHandlerLoggerT :: (Functor m, Monad m) => HandlerLogger m b -> m b
runHandlerLoggerT = fmap fst . flip runStateT topHandler2 . fromHandlerLogger


--runHandlerLoggerT' :: (Functor m, Monad m) => l -> HandlerLogger (MapRTuple Data (Tuple2RTuple l)) m b -> m b
--runHandlerLoggerT' _ = runHandlerLoggerT

--type instance LogFormat (HandlerLogger m) = LogFormat m


getTopHandler = HandlerLogger State.get
putTopHandler = HandlerLogger . State.put

instance (Monad m, Functor m) => MonadLogger (HandlerLogger m) where
    appendLog l =  (runHandler2 l =<< getTopHandler)
                -- *> lift (appendLog l)


runHandler2 :: (Applicative m, Monad m) => Log l -> Handler2 m l -> m ()
runHandler2 l h = (h^.action2) l <* mapM (runHandler2 l) (h^.children2)


instance (Monad m, Functor m, l~LogFormat m) => MonadLoggerHandler (Handler2 (HandlerLogger m) l) (HandlerLogger m) where
    addHandler h = do
        topH <- getTopHandler
        putTopHandler $ addChildHandler2 h topH

instance (Functor m, Monad m, LogBuilder d (HandlerLogger m) (LogFormat m)) => LogConstructor d (HandlerLogger m) where
    mkLog = defMkLog

--instance (Monad m, Applicative m) => MonadLoggerHandler (HandlerWrapper s (BaseLoggerT s m)) (BaseLoggerT s m) where
--    addHandler (HandlerWrapper h) = withLogState (withHandlerWrapper $ addChildHandler h)

----------------------------------------------------------------------
-- Other
----------------------------------------------------------------------




debug :: (LogConstructor (Data Lvl, (Data Msg, ())) m) => String -> m ()
debug     = simpleLog Debug
info      = simpleLog Info
notice    = simpleLog Notice
warning   = simpleLog Warning
error     = simpleLog Error
critical  = simpleLog Critical
alert     = simpleLog Alert
panic     = simpleLog Panic



--flushM = flushMe =<< get



simpleLog = log empty

log :: (Show pri, Enum pri, LogConstructor (Data Lvl, (Data Msg, r)) m)
    => RecordBuilder r -> pri -> String -> m ()
log d pri msg = do
#ifdef NOLOGS
    return ()
#else
    mkLog $ appData Lvl (mkLevel pri)
          $ appData Msg msg
          $ d
#endif








-------------------------------------------------------------------------


newtype PriorityLoggerT m a = PriorityLoggerT { fromPriorityLoggerT :: StateT Int m a } deriving (Monad, MonadIO, Applicative, Functor, MonadTrans)

--type instance LogFormat (PriorityLoggerT m) = LogFormat m


runPriorityLoggerT pri = fmap fst . flip runStateT (fromEnum pri) . fromPriorityLoggerT



class MonadPriorityLogger m where
    getPriority :: m Int
    setPriority :: Enum a => a -> m ()

instance Monad m => MonadPriorityLogger (PriorityLoggerT m) where
    getPriority   = PriorityLoggerT State.get
    setPriority a = PriorityLoggerT . State.put $ fromEnum a

instance (MonadLogger m, LogConstructor d m, LookupDataSet Lvl d) => LogConstructor d (PriorityLoggerT m) where
    mkLog d = do
        priLimit <- getPriority
        let LevelData pri _ = readData Lvl d
        if priLimit <= pri then lift $ mkLog d
                           else return ()

instance (Monad m, MonadLoggerHandler h m) => MonadLoggerHandler h (PriorityLoggerT m) where
    addHandler = defAddHandler


-------------------------------------------------------------------------


newtype ThreadedLogger' d r m a = ThreadedLogger' { fromThreadedLogger :: ReaderT (InChan (ChMsg d r)) m a } deriving (Monad, MonadIO, Applicative, Functor, MonadTrans)

type ThreadedLogger d m a = ThreadedLogger' d a m a

data ChMsg m a = ChMsg (m ()) | End a | Exc SomeException



runThreadedLogger :: (MonadIO m, Applicative m) => ThreadedLogger m (BaseLoggerT l IO) a -> m a
runThreadedLogger m = do
    (inChan, outChan) <- liftIO newChan
    liftIO $ forkIO $ do
        -- cutting out all the logs and sending them over channel, computing result
        out <- (End <$> (runRawBaseLoggerT $ flip runReaderT inChan . fromThreadedLogger $ m)) `catch` (\e -> return (Exc e))
        writeChan inChan (out)
    loop outChan
    where loop :: (MonadIO m, Applicative m) => OutChan (ChMsg m a) -> m a
          loop ch = do
              l <- liftIO $ readChan ch
              case l of
                  End   a -> return a
                  ChMsg d -> d *> loop ch
                  Exc   e -> liftIO $ throwIO e

getChan = ThreadedLogger' Reader.ask

withTarget f = do
    ch <- getChan
    liftIO $ writeChan ch (ChMsg f)

instance (MonadIO m, LogConstructor d n) => LogConstructor d (ThreadedLogger' n a m) where
    mkLog = withTarget . mkLog

instance (MonadIO m, MonadLoggerHandler h d) => MonadLoggerHandler h (ThreadedLogger' d a m) where
    addHandler = withTarget . addHandler

instance (MonadIO m, MonadPriorityLogger d) => MonadPriorityLogger (ThreadedLogger' d a m) where
    setPriority = withTarget . setPriority
    getPriority = P.error "Cannot get priority from within ThreadLogger!"

--instance 

-------------------------------------------------------------------------


test = do
    --let h = flushHandler `addChildHandler` printHandler
    addHandler printHandler2
    debug "running subrutine"

    --x <- runWriterLoggerT $ runDupLogger test2

    --liftIO $ print x

    addHandler printHandler2


    debug "debug1"
    setPriority Debug

    debug "debug2"
    info "info"
    warning "warning"



    --flush
    liftIO $ print "---"
    fail "oh no"
    --liftIO $ threadDelay 1000000
    liftIO $ print "---"


    critical "ola"

    --mkLog (0::Int) "hello"

    return 11




--class LogConstructor d m where
    --mkLog :: RecordBuilder d -> m ()

--instance Num Level


test2 = do
    debug "debug2"

    return 1

main = do
    --print =<< (runBaseLoggerT (Lvl, Msg) $ runHandlerLoggerT $ runPriorityLoggerT Warning test)
    --print =<< runThreadedLogger (runBaseLoggerT (Lvl,Msg) . runHandlerLoggerT . runPriorityLoggerT Warning) test

    --print $ runIdentity (runBaseLoggerT (Lvl, Msg) test)
    --print =<< (runBaseLoggerT (Lvl, Msg) . runThreadedLogger $ test)

    print =<< (runBaseLoggerT (Lvl, Msg, Time) . runHandlerLoggerT . runPriorityLoggerT Warning . runThreadedLogger $ test)
    --print =<< (runBaseLoggerT (Lvl, Msg) . runThreadedLogger $ test2)

    

    --print "end"
    --threadDelay 1000

    --print =<< (runPriorityLoggerT 1 test)
    --print =<< (runBaseLoggerT (test :: StdLogger IO () ))

    --print =<< (fmap snd $ runWriterLoggerT (test2 :: StdLogger2 IO () ))
    return ()





data OneTuple a = OneTuple a deriving Show


--class RTupleConv r t | t -> r, r -> t where
--    t2r :: t -> r
--    r2t :: r -> t

--instance RTupleConv () () where
--    t2r = id
--    r2t = id
--instance RTupleConv (a,()) (OneTuple a) where
--    t2r (OneTuple a) = (a,())
--    r2t (a,()) = OneTuple a
--instance RTupleConv (t1,(t2,())) (t1,t2) where
--    t2r (t1,t2) = (t1,(t2,()))
--    r2t (t1,(t2,())) = (t1,t2)
--instance RTupleConv (t1,(t2,(t3,()))) (t1,t2,t3) where
--    t2r (t1,t2,t3) = (t1,(t2,(t3,())))
--    r2t (t1,(t2,(t3,()))) = (t1,t2,t3)
--instance RTupleConv (t1,(t2,(t3,(t4,())))) (t1,t2,t3,t4) where
--    t2r (t1,t2,t3,t4) = (t1,(t2,(t3,(t4,()))))
--    r2t (t1,(t2,(t3,(t4,())))) = (t1,t2,t3,t4)
--instance RTupleConv (t1,(t2,(t3,(t4,(t5,()))))) (t1,t2,t3,t4,t5) where
--    t2r (t1,t2,t3,t4,t5) = (t1,(t2,(t3,(t4,(t5,())))))
--    r2t (t1,(t2,(t3,(t4,(t5,()))))) = (t1,t2,t3,t4,t5)
--instance RTupleConv (t1,(t2,(t3,(t4,(t5,(t6,())))))) (t1,t2,t3,t4,t5,t6) where
--    t2r (t1,t2,t3,t4,t5,t6) = (t1,(t2,(t3,(t4,(t5,(t6,()))))))
--    r2t (t1,(t2,(t3,(t4,(t5,(t6,())))))) = (t1,t2,t3,t4,t5,t6)
--instance RTupleConv (t1,(t2,(t3,(t4,(t5,(t6,(t7,()))))))) (t1,t2,t3,t4,t5,t6,t7) where
--    t2r (t1,t2,t3,t4,t5,t6,t7) = (t1,(t2,(t3,(t4,(t5,(t6,(t7,())))))))
--    r2t (t1,(t2,(t3,(t4,(t5,(t6,(t7,()))))))) = (t1,t2,t3,t4,t5,t6,t7)
--instance RTupleConv (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,())))))))) (t1,t2,t3,t4,t5,t6,t7,t8) where
--    t2r (t1,t2,t3,t4,t5,t6,t7,t8) = (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,()))))))))
--    r2t (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,())))))))) = (t1,t2,t3,t4,t5,t6,t7,t8)
--instance RTupleConv (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,()))))))))) (t1,t2,t3,t4,t5,t6,t7,t8,t9) where
--    t2r (t1,t2,t3,t4,t5,t6,t7,t8,t9) = (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,())))))))))
--    r2t (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,()))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9)
--instance RTupleConv (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,())))))))))) (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) where
--    t2r (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) = (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,()))))))))))
--    r2t (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,())))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)





-- TODO:
--   filters
--   proper handler formatting