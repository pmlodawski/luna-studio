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

import Prelude hiding (log, lookup)
import Data.String.Class (ToString(toString))
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
import Control.Lens hiding ((|>))
import Control.Monad (when)
import Data.Foldable (toList)

import Data.Time (getCurrentTime)
--class MonadLogger t m where
--  log :: Logger t -> t -> m ()


--instance ToString a => MonadLogger a IO where
--  log _ = print . toString



data Message a = Message {fromMessage :: a } deriving (Show, Functor) -- { _msg      :: a 
                         --, _logPath  :: [Text]
                         --, _priority :: Int
                         --, _threadID :: Int
                         --, _procID   :: Int
                         --, _time     :: Int
                         -- } deriving (Show)


--data Segment = Msg
--             | Name
--             | Priority
--             | ThreadID
--             | ProcessID
--             -- | Time -- <timeFormatter>
--             deriving (Show)

-- dorobic IsString do Segmentu

data Priority = DEBUG     -- ^ Debug messages
              | INFO      -- ^ Information
              | NOTICE    -- ^ Normal runtime conditions
              | WARNING   -- ^ General Warnings
              | ERROR     -- ^ General Errors
              | CRITICAL  -- ^ Severe situations
              | ALERT     -- ^ Take immediate action
              | EMERGENCY -- ^ System is unusable
              deriving (Eq, Ord, Show, Read, Enum)

data Msg  = Msg String deriving (Show)
data Pri  = Pri Int deriving (Show)



newtype OneTuple a = OneTuple a deriving (Show)

data Logging = Logging deriving (Show)


data Handler s m = forall a. (Show a, LogHandler a m s) => 
     Handler { h2_1      :: a
             , _handlers :: [Handler s m]
             , _priority :: Int
             }

type MsgHandler s m = Message s -> m ()


instance Show (Handler s m) where
    show (Handler a _ pr) = "Handler " <> show a <> " " <> show pr


data TopHandler = TopHandler deriving (Show)

topHandler = mkHandler TopHandler

instance (Monad m, Applicative m, Show msg) => LogHandler TopHandler m msg where
    handle h _ = return h

data PrintHandler = PrintHandler deriving (Show)

instance (MonadIO m, Applicative m, Show msg) => LogHandler PrintHandler m msg where
    handle h msg = do
        liftIO $ print msg
        return h

printHandler = mkHandler PrintHandler


class Flush s m where
    flushH :: Handler s m -> m ()

    default flushH :: Monad m => Handler s m -> m ()
    flushH _ = return ()

--instance Monad m => Flush TopHandler m
--instance Monad m => Flush PrintHandler m
--instance Monad m => Flush (FlushHandler s) m where
    --flushH (FlushHandler msgs) = (\msg -> mapM (flip runHandler msg) hs)

data FlushHandler s = FlushHandler [Message s] deriving (Show)

instance (Monad m, Applicative m, Show msg, msg~msg') => LogHandler (FlushHandler msg) m msg' where
    handle (FlushHandler msgs) msg = return (FlushHandler (msg:msgs))
    subhandle _ hs _ = return hs

flushHandler = mkHandler (FlushHandler [])

mkHandler base = Handler base [] 0

runHandler :: (Functor m, Applicative m, Monad m) => Handler s m -> Message s -> m (Handler s m)
runHandler (Handler a hs pr) msg = Handler <$> (handle a msg)
                                           <*> subhandle a hs msg
                                           <*> pure pr


instance (Show s, Monad m, Applicative m) => Monoid (Handler s m) where
    mempty = topHandler


class LogHandler h m msg where
    handle :: h -> Message msg -> m h
    subhandle :: h -> [Handler msg m] -> Message msg -> m [Handler msg m]

    default subhandle :: (Monad m, Applicative m) => h -> [Handler msg m] -> Message msg -> m [Handler msg m]
    subhandle _ hs msg = mapM (flip runHandler msg) hs

newtype LoggerT s m a = LoggerT { unLoggerT :: StateT (Handler s (LoggerT s m)) m a } deriving (Monad, MonadIO, Applicative, Functor)
type    Logger s a = LoggerT s Identity a

--data Logger t = Logger deriving (Show)

class Monad m => MonadLogger s m | m -> s where
    get   :: m (Handler s m)
    put   :: Handler s m -> m ()
    --appendMsg :: Message s -> m ()
    --getInt :: m Int

--instance Flush (LoggerT s m) where
--    flush = flush =<< get

flush = flushH =<< get

--instance Monad m => Flush (Handler s m) m where
--    flushH (Handler a _ _) = flushH a

newtype LogBuilder a = LogBuilder { fromLogBuilder :: a } deriving (Show, Functor)

runLoggerT = flip (runStateT . unLoggerT) mempty
runLogger  = runIdentity . runLoggerT

withLogState f = do
    s <- get
    put $ f s

instance Monad m => MonadLogger s (LoggerT s m) where
    get   = LoggerT State.get
    put   = LoggerT . State.put

    --flush = put mempty

--instance (Functor m, Monad m) => Flush (LoggerT s m) where
--    flush = do -- put mempty
--        --hs <- getHandlers
--        return ()
--        --mapM_ flush hs
--        put mempty

--appendMsg a = withLogState (msgs %~ (<> return a))
addHandler h = withLogState (addChildHandler h)

addChildHandler h s = s { _handlers = h : _handlers s}

--getFilters  = view filters  <$> get

getLvl  = _priority <$> get
--getMsgs = view msgs <$> get
getHandlers = _handlers <$> get



--type StringLogger = Logger String



--class AppData d bldr out | d bldr -> out where
--    appData :: d -> bldr -> out

--instance AppData d a b => AppData d (LogBuilder a) (LogBuilder b) where
--    appData d = fmap (appData d)

appData :: a -> LogBuilder as -> LogBuilder (a,as)
appData = fmap . (,)

class MessageBuilder a m b where
    buildMessage :: LogBuilder a -> m (Message b)


instance (MessageBuilder xs m ys, Functor m) => MessageBuilder (x,xs) m (x,ys) where
    buildMessage b = (fmap.fmap) (x,) $ buildMessage $ LogBuilder xs where
        (x,xs) = fromLogBuilder b


instance (MessageBuilder (x,xs) m ys, MessageBuilder xs m (y,()), Monad m) => MessageBuilder (x,xs) m (y,ys) where
    buildMessage b = do
        let (x,xs) = fromLogBuilder b
        Message ys     <- buildMessage b
        Message (y,()) <- buildMessage $ LogBuilder xs
        return $ Message (y, ys)
        

instance Monad m => MessageBuilder a m () where
    buildMessage _ = return $ Message ()

instance (Functor m, Applicative m, DataGetter m y, MessageBuilder () m ys) => MessageBuilder () m (y,ys) where
    buildMessage b = fmap Message $ (,) <$> getData <*> (fromMessage <$> buildMessage b)


class Lookup s a where 
    lookup :: s -> a

instance Lookup (a,as) a where
    lookup (a,_) = a

instance Lookup as a => Lookup (b,as) a where
    lookup (_, as) = lookup as

readData :: Lookup a b => Message a -> b
readData = lookup . fromMessage

readMsg :: Lookup a Msg => Message a -> Msg
readMsg = readData

--instance MessageBuilder as (b,()) => MessageBuilder (a,as) (b,()) where
--    buildMessage = buildMessage . fmap snd

data Time = Time deriving (Show)

instance MonadIO m => DataGetter m Time where
    getData = do
        liftIO $ print "reading time"
        return Time

empty = LogBuilder ()

--class MessageFilter where
--    filterMessage :: (a ->)

class DataGetter m d where
    getData :: m d


-- getFilters? funkcja monadyczna ?
log pri msg = do
    let lvl = fromEnum pri
    tlvl <- getLvl
    when (lvl >= tlvl) $ do
        msg <- buildMessage
             $ appData (Pri lvl)
             $ appData (Msg msg)
             $ empty
        --appendMsg msg
        --return ()
        runMessage msg

runMessage msg = do
    hs   <- getHandlers
    mapM (flip runHandler msg) $ hs
    return ()

debug     = log DEBUG
info      = log INFO
notice    = log NOTICE
warning   = log WARNING
error     = log ERROR
critical  = log CRITICAL
alert     = log ALERT
emergency = log EMERGENCY

test = do
    let h = addChildHandler printHandler flushHandler
    addHandler h
    --addHandler flushHandler
    debug "ala"
    --addHandler printHandler

    --flush
    critical "ola"

    return ()



type StdLogger m a = LoggerT (Msg, (Pri,())) m a

main = do
    --print $ (runLogger (test :: StdLogger Identity ())   )
    print =<< (snd <$> runLoggerT (test :: StdLogger IO ())   )
    --print =<< getCurrentTime
    --let l = Logger :: StringLogger
    --log l "ala"
    return ()



--dorobic filtracje - ale madra! jezeli cos nie wymaga IO (jak np. filtracja po priority) to powinna byc robiona
--przed IO
--mozna to zrobic tak, ze liczymyu dane ktore sa potrzebne do filtracji i aplikujemy je na LogBuilderze
--jak normalne appData - jezeli nie zostaly znalezione!