{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}

import Prelude hiding (log)
import Data.String.Class (ToString(toString))
import Control.Monad.State
import Control.Applicative
import Data.Generics (Generic)
import Data.Monoid
import Control.Monad.Identity (Identity, runIdentity)
import Data.Sequence (Seq)
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy (Text)
--class MonadLogger t m where
--  log :: Logger t -> t -> m ()


--instance ToString a => MonadLogger a IO where
--  log _ = print . toString



data Message a = Message { _msg      :: a 
                         --, _logPath  :: [Text]
                         , _priority :: Int
                         --, _threadID :: Int
                         --, _procID   :: Int
                         --, _time     :: Int
                         } deriving (Show)

-- te rzeczy (szczegolnie IO) musimy odczytywac tylko jak tego potrzebujemy
-- jak nie ma handlerów to co? Mogą być doczepione przed flushowaniem (!)

-- warto zrobic limity wypisywania nie tylko na handlerach ale na całych loggerach!

-- moze message zamienic na komponowalną tuplę? i wyciagac z niej elementy bazując na typach?
-- wtedy tez mozna tworzyc wlasne segmenty (!)

-- np. typ: Message (Msg, Name, Priority)

data Segment = Msg
             | Name
             | Priority
             | ThreadID
             | ProcessID
             -- | Time -- <timeFormatter>
             deriving (Show)

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

debug     = log DEBUG
info      = log INFO
notice    = log NOTICE
warning   = log WARNING
error     = log ERROR
critical  = log CRITICAL
alert     = log ALERT
emergency = log EMERGENCY


data Logging = Logging deriving (Show)

newtype LoggerT s m a = LoggerT { unLoggerT :: StateT (Seq (Message s)) m a } deriving (Monad, MonadIO, Applicative, Functor)
type    Logger s a = LoggerT s Identity a

--data Logger t = Logger deriving (Show)

class Monad m => MonadLogger s m | m -> s where
    log :: Enum prio => prio -> s -> m ()


runLoggerT = flip (runStateT . unLoggerT) mempty
runLogger  = runIdentity . runLoggerT

instance Monad m => MonadLogger s (LoggerT s m)  where
    log pri a = LoggerT $ do
        s <- get
        put $ s <> return (Message a (fromEnum pri))

--type StringLogger = Logger String

test = do
    debug "ala"
    debug "ala"

main = do
    print =<< runLoggerT test
    --let l = Logger :: StringLogger
    --log l "ala"
    return ()

