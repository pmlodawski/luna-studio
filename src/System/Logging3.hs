{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}

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

-- te rzeczy (szczegolnie IO) musimy odczytywac tylko jak tego potrzebujemy
-- jak nie ma handlerów to co? Mogą być doczepione przed flushowaniem (!)

-- warto zrobic limity wypisywania nie tylko na handlerach ale na całych loggerach!

-- moze message zamienic na komponowalną tuplę? i wyciagac z niej elementy bazując na typach?
-- wtedy tez mozna tworzyc wlasne segmenty (!)

-- np. typ: Message (Msg, Name, Priority)

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
data Time = Time deriving (Show)


newtype OneTuple a = OneTuple a deriving (Show)

data Logging = Logging deriving (Show)

data Handler s m = Handler { fromHandler :: Handler s m -> Message s -> m (Handler s m) }

runHandler msg h = fromHandler h h msg

printHandler :: (MonadIO m, Applicative m, Monad m, Show s) => Handler s m
printHandler = Handler (\h msg -> liftIO (print msg) *> return h)

data Handler2 = Handler2

data LogState s m = LogState { _msgs     :: Seq (Message s) 
                             , _handlers :: [Handler s m ]
                             , _lvl      :: Int
                             }


instance Monoid (LogState s m) where
    mempty = LogState mempty mempty 0
    --(LogState m f) `mappend` (LogState m' f') = LogState (m `mappend` m')
    --                                                     (f `mappend` f')

makeLenses ''LogState

newtype LoggerT s m a = LoggerT { unLoggerT :: StateT (LogState s (LoggerT s m)) m a } deriving (Monad, MonadIO, Applicative, Functor)
type    Logger s a = LoggerT s Identity a

--data Logger t = Logger deriving (Show)

class Monad m => MonadLogger s m | m -> s where
    get   :: m (LogState s m)
    put   :: LogState s m -> m ()
    --appendMsg :: Message s -> m ()
    --getInt :: m Int

class Flush m where
    flush :: m ()

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

instance (Functor m, Monad m) => Flush (LoggerT s m) where
    flush = do -- put mempty
        hs <- getHandlers
        return ()
        --mapM_ flush hs
        put mempty

appendMsg a = withLogState (msgs %~ (<> return a))
addHandler h = withLogState (handlers %~ (h:))

--getFilters  = view filters  <$> get

getLvl  = view lvl  <$> get
getMsgs = view msgs <$> get
getHandlers = view handlers <$> get

runMessage msg = do
    hs <- getHandlers
    mapM (runHandler msg) hs
    return ()

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
        appendMsg msg
        runMessage msg

debug     = log DEBUG
info      = log INFO
notice    = log NOTICE
warning   = log WARNING
error     = log ERROR
critical  = log CRITICAL
alert     = log ALERT
emergency = log EMERGENCY

test = do
    addHandler printHandler
    debug "ala"
    --flush
    critical "ola"

    return ()



type StdLogger m a = LoggerT (Msg, (Pri,(Time,()))) m a

main = do
    print =<< (view msgs . snd <$> runLoggerT (test :: StdLogger IO ())   )
    --print =<< getCurrentTime
    --let l = Logger :: StringLogger
    --log l "ala"
    return ()



--dorobic filtracje - ale madra! jezeli cos nie wymaga IO (jak np. filtracja po priority) to powinna byc robiona
--przed IO
--mozna to zrobic tak, ze liczymyu dane ktore sa potrzebne do filtracji i aplikujemy je na LogBuilderze
--jak normalne appData - jezeli nie zostaly znalezione!