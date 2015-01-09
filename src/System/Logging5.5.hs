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

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude hiding (log, lookup)
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
import Control.Lens hiding ((|>), children, Level)
import Control.Monad (when)
import Data.Foldable (toList)

import Data.Time (getCurrentTime)
import Data.Typeable
import System.IO (stdout)
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>), empty)


newtype Log a = Log { unLog :: a } deriving (Show, Functor) -- { _msg      :: a 
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

class DataGetter m d where
    getData :: m d





data Time = Time deriving (Show)

instance MonadIO m => DataGetter m Time where
    getData = do liftIO $ print "reading time"
                 return Time



--class LogFilter where
--    filterLog :: (a ->)



data Msg = Msg deriving (Show)
type instance DataOf Msg = String

data Level = Level Int String deriving (Show, Ord, Eq)

mkLevel a = Level (fromEnum a) (show a)

data Lvl = Lvl deriving (Show)
type instance DataOf Lvl = Level


data DataRecord base = DataRecord { recBase :: base
                                  , recData :: DataOf base
                                  }

deriving instance (Show (DataOf base), Show base) => Show (DataRecord base)

type family DataOf a :: *

--data DataProvider base d = 

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

data PrintHandler f = PrintHandler (Formatter f) deriving (Show)

instance (MonadIO m, Applicative m, f~msg) => LogHandler (PrintHandler f) m msg where
    handle h msg = do
        let PrintHandler formatter = view base h
        liftIO $ putDoc $ runFormatter formatter msg
        liftIO $ putStrLn ""
        runChildren h msg

printHandler = mkHandler (PrintHandler foo)



runChildren h msg = do
    let ch = view children h
    hs <- mapM (flip runHandler msg) ch
    return (h & children .~ hs)


----------------------------------------------------------------------
-- Data reading
----------------------------------------------------------------------

class Lookup base s where 
    lookup :: base -> s -> DataRecord base

instance Lookup base (DataRecord base,as) where
    lookup _ (a,_) = a

instance Lookup base as => Lookup base (DataRecord b,as) where
    lookup b (_, as) = lookup b as

readData :: Lookup a l => a -> Log l -> DataOf a
readData a = recData . lookup a . unLog


----------------------------------------------------------------------
-- Formatters
----------------------------------------------------------------------

data Formatter a = Formatter { runFormatter :: Log a -> Doc }

instance Show (Formatter a) where
    show _ = "Formatter"

--read log = readData

--foo = Formatter $ show . readData Msg

foo = colorLvlFormatter ("[" <:> Lvl <:> "] ") <:> Msg <:> " !"

colorLvlFormatter f = Formatter (\s -> let (Level pr _) = readData Lvl s in lvlColor pr $ runFormatter f s)

lvlColor lvl
    | lvl == 0  = id
    | lvl <= 2  = green
    | lvl == 3  = yellow
    | otherwise = red



mapFormatter f (Formatter a) = Formatter (f a)

(<:>) :: (FormatterBuilder a c, FormatterBuilder b c) => a -> b -> Formatter c
(<:>) a b = concatFormatters (buildFormatter a) (buildFormatter b)

concatFormatters :: Formatter a -> Formatter a -> Formatter a
concatFormatters (Formatter f) (Formatter g) = Formatter (\s -> f s <> g s)

class FormatterBuilder a b where
    buildFormatter :: a -> Formatter b

instance FormatterBuilder String a where
    buildFormatter a = Formatter $ const (text a)

instance FormatterBuilder Doc a where
    buildFormatter a = Formatter $ const a

--instance Lookup Lvl a => FormatterBuilder Lvl (Formatter a) where
--    buildFormatter a = Formatter $ pprint . readData a

instance (Lookup seg a, PPrint (DataOf seg)) => FormatterBuilder seg a where
    buildFormatter a = Formatter $ pprint . readData a

instance (a~b) => FormatterBuilder (Formatter a) b where
    buildFormatter = id

class PPrint a where
    pprint :: a -> Doc

instance PPrint String where
    pprint = text

instance Pretty a => PPrint a where
    pprint = pretty

instance Pretty Level where
    pretty (Level _ name) = text name

--class FormatterBuilder a b c where
--    buildFormatter :: a -> b -> c

--instance FormatterBuilder String String (Formatter a) where
--    buildFormatter a b = Formatter (const a) `buildFormatter` Formatter (const b)

--instance (a~b, a~c) => FormatterBuilder (Formatter a) (Formatter b) (Formatter c) where
--    buildFormatter (Formatter f) (Formatter g) = Formatter (\s -> f s <> g s)

--instance Lookup a l => FormatterBuilder String a b where
--    buildFormatter s a = Formatter (const s) `buildFormatter` Formatter (show . readData a)
    

----------------------------------------------------------------------
-- Other
----------------------------------------------------------------------

runHandler :: (Functor m, Applicative m, Monad m) => HandlerWrapper s m -> Log s -> m (HandlerWrapper s m)
runHandler (HandlerWrapper h) msg = fmap HandlerWrapper $ handle h msg



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
empty = RecordBuilder ()

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

    --appData :: a -> RecordBuilder as -> RecordBuilder (a,as)
    --appData = fmap . (,)

    --appData2 :: (a~DataOf base) => base -> a -> RecordBuilder as -> RecordBuilder (a,as)
    --appData2 _ = fmap . (,)

appData :: (a~DataOf base) => base -> a -> RecordBuilder as -> RecordBuilder (DataRecord base, as)
appData base a = fmap (DataRecord base a,)

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
    buildLog b = fmap Log $ (,) <$> getData <*> (unLog <$> buildLog b)





--instance LogBuilder as (b,()) => LogBuilder (a,as) (b,()) where
--    buildLog = buildLog . fmap snd




-- getFilters? funkcja monadyczna ?
log pri msg = do
    let lvl = mkLevel pri
    tlvl <- return $ mkLevel (0 :: Int) -- getLvl
    when (lvl >= tlvl) $ do
        msg <- buildLog
             $ appData Lvl lvl
             $ appData Msg msg
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
    debug "debug"
    info "info"
    warning "warning"

    liftIO $ print "---"

    critical "ola"
    flushM

    return ()


data OneTuple a = OneTuple a deriving Show


class RTupleConv t r | t -> r, r -> t where

instance RTupleConv () ()
instance RTupleConv (a,()) (OneTuple a)
instance RTupleConv (t1,(t2,())) (t1,t2)
instance RTupleConv (t1,(t2,(t3,()))) (t1,t2,t3)
instance RTupleConv (t1,(t2,(t3,(t4,())))) (t1,t2,t3,t4)
instance RTupleConv (t1,(t2,(t3,(t4,(t5,()))))) (t1,t2,t3,t4,t5)
instance RTupleConv (t1,(t2,(t3,(t4,(t5,(t6,())))))) (t1,t2,t3,t4,t5,t6)
instance RTupleConv (t1,(t2,(t3,(t4,(t5,(t6,(t7,()))))))) (t1,t2,t3,t4,t5,t6,t7)
instance RTupleConv (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,())))))))) (t1,t2,t3,t4,t5,t6,t7,t8)
instance RTupleConv (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,()))))))))) (t1,t2,t3,t4,t5,t6,t7,t8,t9)
instance RTupleConv (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,())))))))))) (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)

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


type family Insert t set where
  Insert t ()    = (t,())
  Insert t (t,x) = (t,x)
  Insert t (a,x) = (a,Insert t x)


type family MapRTuple (f :: * -> *) tup where
    MapRTuple f () = ()
    MapRTuple f (a,as) = (f a, MapRTuple f as)


type StdLogger m a = LoggerX (Lvl, Msg) m a

type StdTypes s = Insert Lvl (Insert Msg s) 

type LoggerX s m a = LoggerT (MapRTuple DataRecord (StdTypes (Tuple2RTuple s))) m a


main = do
    --print $ (runLogger (test :: StdLogger Identity ())   )
    print =<< (snd <$> runLoggerT (test :: StdLogger IO ())   )
    --print =<< getCurrentTime
    --let l = Logger :: StringLogger
    --log l "ala"

    --print $ typeOf (undefined :: Equals Int a)
    --return ()


--class CheckData k v set out | k v set -> out where

--data True  deriving (Typeable)
--data False deriving (Typeable)

--type family Equals a b where
--    Equals a a = True
--    Equals a b = False



--instance                      CheckData k v ()         ((k,v),())
--instance k~k'              => CheckData k v ((k,v'),s) ((k,v),())
--instance CheckData k v s t => CheckData k v ((l,v'),s) ((l,v'),t)

--dorobic filtracje - ale madra! jezeli cos nie wymaga IO (jak np. filtracja po priority) to powinna byc robiona
--przed IO
--mozna to zrobic tak, ze liczymyu dane ktore sa potrzebne do filtracji i aplikujemy je na RecordBuilderze
--jak normalne appData - jezeli nie zostaly znalezione!