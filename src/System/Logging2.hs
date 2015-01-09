{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverlappingInstances #-}

import Prelude hiding (log)
import Data.String.Class (ToString(toString))
import Control.Monad.State
import Control.Applicative hiding (empty)
import Data.Generics (Generic)
import Data.Monoid
import Control.Monad.Identity (Identity, runIdentity)
import Data.Sequence (Seq)
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy (Text)

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

data Msg a = Msg a deriving (Show)
data Pri   = Pri Int deriving (Show)
data Time = Time deriving (Show)


newtype OneTuple a = OneTuple a deriving (Show)

data Logging = Logging deriving (Show)

newtype LoggerT s m a = LoggerT { unLoggerT :: StateT (Seq (Message s)) m a } deriving (Monad, MonadIO, Applicative, Functor)
type    Logger s a = LoggerT s Identity a

--data Logger t = Logger deriving (Show)

class Monad m => MonadLogger s m | m -> s where
    appendMsg :: Message s -> m ()

newtype LogBuilder a = LogBuilder { fromLogBuilder :: a } deriving (Show, Functor)

runLoggerT = flip (runStateT . unLoggerT) mempty
runLogger  = runIdentity . runLoggerT

instance Monad m => MonadLogger s (LoggerT s m)  where
    appendMsg a = LoggerT $ do
        s <- get
        put $ s <> return a

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

--instance MessageBuilder as (b,()) => MessageBuilder (a,as) (b,()) where
--    buildMessage = buildMessage . fmap snd

instance MonadIO m => DataGetter m Time where
    getData = return Time

empty = LogBuilder ()

class DataGetter m d where
    getData :: m d

log pri msg = appendMsg =<< (buildMessage
            $ appData (Pri $ fromEnum pri)
            $ appData (Msg msg)
            $ empty)

debug     = log DEBUG
info      = log INFO
notice    = log NOTICE
warning   = log WARNING
error     = log ERROR
critical  = log CRITICAL
alert     = log ALERT
emergency = log EMERGENCY

test = do
    debug "ala"
    critical "ola"
    return ()



main = do
    print =<< runLoggerT (test :: LoggerT (Msg String,(Pri,())) IO ())
    --print =<< getCurrentTime
    --let l = Logger :: StringLogger
    --log l "ala"
    return ()



--dorobic filtracje - ale madra! jezeli cos nie wymaga IO (jak np. filtracja po priority) to powinna byc robiona
--przed IO
--mozna to zrobic tak, ze liczymyu dane ktore sa potrzebne do filtracji i aplikujemy je na LogBuilderze
--jak normalne appData - jezeli nie zostaly znalezione!