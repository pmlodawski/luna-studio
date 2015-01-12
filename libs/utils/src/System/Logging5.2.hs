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
import Control.Lens hiding ((|>), children)
import Control.Monad (when)
import Data.Foldable (toList)

import Data.Time (getCurrentTime)



data Log a = Log { fromLog :: a } deriving (Show, Functor) -- { _msg      :: a 
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



data Msg  = Msg String deriving (Show)
data Pri  = Pri Int deriving (Show)


----------------------------------------------------------------------
-- LogHandler
----------------------------------------------------------------------

class LogHandler h m msg where
    handle :: Handler msg m h -> Log msg -> m (Handler msg m h)
    flush  :: Handler msg m h -> m (Handler msg m h)
    close  :: Handler msg m h -> m ()

    default flush :: (Monad m, Functor m) => Handler msg m h -> m (Handler msg m h)
    flush = defaultFlush

    default close :: (Monad m, Applicative m, Functor m) => Handler msg m h -> m ()
    close h = flush h *> pure ()


defaultFlush h = do
    ch <- mapM flushMe $ _children h
    return $ h { _children = ch }

flushMe (HandlerWrapper h) = fmap HandlerWrapper $ flush h


----------------------------------------------------------------------
-- Handler & HandlerWrapper
----------------------------------------------------------------------

mkHandler base = Handler base [] Nothing

data Handler s m a = Handler { _base     :: a
                             , _children :: [HandlerWrapper s m]
                             , _level    :: Maybe Int
                             }

data HandlerWrapper s m = forall a. (Show a, LogHandler a m s) => HandlerWrapper (Handler s m a)

withHandlerWrapper :: (forall a. (Show a, LogHandler a m s) => Handler s m a -> Handler s m a) -> HandlerWrapper s m -> HandlerWrapper s m
withHandlerWrapper f (HandlerWrapper h) = HandlerWrapper $ f h

makeLenses ''Handler


instance Show (HandlerWrapper s m) where
    show (HandlerWrapper h) = show h

instance Show a => Show (Handler s m a) where
    show (Handler a _ pr) = "Handler " <> show a <> " " <> show pr


----------------------------------------------------------------------
-- Handlers
----------------------------------------------------------------------

-- Top Handler

data TopHandler = TopHandler deriving (Show)

topHandler = mkHandler TopHandler

instance (Monad m, Applicative m, Show msg) => LogHandler TopHandler m msg where
    handle h _ = return h


-- Flush Handler

data FlushHandler s = FlushHandler { _buffer :: Seq (Log s) } deriving (Show)

makeLenses ''FlushHandler

enqueueMsg msg = buffer %~ (|> msg)

flushHandler = mkHandler (FlushHandler mempty)

instance (MonadIO m, Applicative m, Show msg, msg~msg') => LogHandler (FlushHandler msg) m msg' where
    handle h msg = return (h & base %~ enqueueMsg msg) -- runChildren h msg
    flush h = do
        mapM (\msg -> mapM (flip runHandler msg) ch) (toList $ view (base.buffer) h) *> defaultFlush h
        where ch = view children h

-- Print Handler

data PrintHandler = PrintHandler deriving (Show)

instance (MonadIO m, Applicative m, Show msg) => LogHandler PrintHandler m msg where
    handle h msg = do
        liftIO $ print msg
        runChildren h msg

printHandler = mkHandler PrintHandler



runChildren h msg = do
    let ch = view children h
    hs <- mapM (flip runHandler msg) ch
    return (h & children .~ hs)





--h2w :: forall a m s. (Show a, LogHandler a m s) => Handler s m -> (Handler s m a)
--h2w (Handler a hs pr) = Handler a hs pr

runHandler :: (Functor m, Applicative m, Monad m) => HandlerWrapper s m -> Log s -> m (HandlerWrapper s m)
runHandler (HandlerWrapper h) msg = fmap HandlerWrapper $ handle h msg

--runHandler (Handler a hs pr) msg = Handler <$> (handle a msg)
--                                           <*> subhandle a hs msg
--                                           <*> pure pr



instance (Show s, Monad m, Applicative m) => Monoid (HandlerWrapper s m) where
    mempty = HandlerWrapper topHandler





newtype LoggerT s m a = LoggerT { unLoggerT :: StateT (HandlerWrapper s (LoggerT s m)) m a } deriving (Monad, MonadIO, Applicative, Functor)
type    Logger s a = LoggerT s Identity a

--data Logger t = Logger deriving (Show)

class Monad m => MonadLogger s m | m -> s where
    get   :: m (HandlerWrapper s m)
    put   :: HandlerWrapper s m -> m ()
    --appendMsg :: Log s -> m ()
    --getInt :: m Int

newtype RecordBuilder a = RecordBuilder { fromRecordBuilder :: a } deriving (Show, Functor)

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
addHandler :: (MonadLogger s m, LogHandler a m s, Show a) => Handler s m a -> m ()
addHandler h = withLogState (withHandlerWrapper $ addChildHandler h)

addChildHandler h s = s & children %~ (HandlerWrapper h:)

--getFilters  = view filters  <$> get

--getLvl  = _priorityx <$> get
--getMsgs = view msgs <$> get
--getHandlers = _handlersx <$> get



--type StringLogger = Logger String



--class AppData d bldr out | d bldr -> out where
--    appData :: d -> bldr -> out

--instance AppData d a b => AppData d (RecordBuilder a) (RecordBuilder b) where
--    appData d = fmap (appData d)

appData :: a -> RecordBuilder as -> RecordBuilder (a,as)
appData = fmap . (,)

class LogBuilder a m b where
    buildLog :: RecordBuilder a -> m (Log b)


instance (LogBuilder xs m ys, Functor m) => LogBuilder (x,xs) m (x,ys) where
    buildLog b = (fmap.fmap) (x,) $ buildLog $ RecordBuilder xs where
        (x,xs) = fromRecordBuilder b


instance (LogBuilder (x,xs) m ys, LogBuilder xs m (y,()), Monad m) => LogBuilder (x,xs) m (y,ys) where
    buildLog b = do
        let (x,xs) = fromRecordBuilder b
        Log ys     <- buildLog b
        Log (y,()) <- buildLog $ RecordBuilder xs
        return $ Log (y, ys)
        

instance Monad m => LogBuilder a m () where
    buildLog _ = return $ Log ()

instance (Functor m, Applicative m, DataGetter m y, LogBuilder () m ys) => LogBuilder () m (y,ys) where
    buildLog b = fmap Log $ (,) <$> getData <*> (fromLog <$> buildLog b)


class Lookup s a where 
    lookup :: s -> a

instance Lookup (a,as) a where
    lookup (a,_) = a

instance Lookup as a => Lookup (b,as) a where
    lookup (_, as) = lookup as

readData :: Lookup a b => Log a -> b
readData = lookup . fromLog

readMsg :: Lookup a Msg => Log a -> Msg
readMsg = readData

--instance LogBuilder as (b,()) => LogBuilder (a,as) (b,()) where
--    buildLog = buildLog . fmap snd

data Time = Time deriving (Show)

instance MonadIO m => DataGetter m Time where
    getData = do
        liftIO $ print "reading time"
        return Time

empty = RecordBuilder ()

--class LogFilter where
--    filterLog :: (a ->)

class DataGetter m d where
    getData :: m d


-- getFilters? funkcja monadyczna ?
log pri msg = do
    let lvl = fromEnum pri
    tlvl <- return 0 -- getLvl
    when (lvl >= tlvl) $ do
        msg <- buildLog
             $ appData (Pri lvl)
             $ appData (Msg msg)
             $ empty
        --appendMsg msg
        --return ()
        runLog msg

runLog msg = do
    (HandlerWrapper h) <- get
    s' <- runChildren h msg
    put $ HandlerWrapper s' 


data Priority = DEBUG     -- ^ Debug Logs
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

flushM = flushMe =<< get

test = do
    let h = addChildHandler printHandler flushHandler
    addHandler h
    --addHandler flushHandler
    addHandler printHandler
    debug "ala"

    critical "ola"
    flushM

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
--mozna to zrobic tak, ze liczymyu dane ktore sa potrzebne do filtracji i aplikujemy je na RecordBuilderze
--jak normalne appData - jezeli nie zostaly znalezione!