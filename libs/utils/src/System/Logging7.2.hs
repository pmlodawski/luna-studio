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
--{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

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
import Control.Lens hiding ((|>), children, LevelData, Level)
import Control.Monad (when)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Foldable (toList)

import Data.Time (getCurrentTime)
import Data.Typeable
import System.IO (stdout)
import Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>), empty)
import Control.Concurrent (threadDelay)

import System.Log.Level


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

class DataGetter base m where
    getData :: m (Data base)





data Time = Time deriving (Show)
type instance DataOf Time = Int

instance MonadIO m => DataGetter Time m where
    getData = do liftIO $ print "reading time"
                 liftIO $ threadDelay 1000000
                 return $ Data Time 5



--class LogFilter where
--    filterLog :: (a ->)



data Msg = Msg deriving (Show)
type instance DataOf Msg = String

data LevelData = LevelData Int String deriving (Show, Ord, Eq)

mkLevel a = LevelData (fromEnum a) (show a)

data Lvl = Lvl deriving (Show)
type instance DataOf Lvl = LevelData


data Data base = Data { recBase :: base
                      , recData :: DataOf base
                      }

deriving instance (Show (DataOf base), Show base) => Show (Data base)

type family DataOf a :: *

--data DataProvider base d = 

----------------------------------------------------------------------
-- LogHandler
----------------------------------------------------------------------

class LogHandler h m msg where
    handle :: Handler msg m h -> Log msg -> m (Handler msg m h)
    flushHandle  :: Handler msg m h -> m (Handler msg m h)
    closeHandle  :: Handler msg m h -> m ()

    default flushHandle :: (Monad m, Functor m) => Handler msg m h -> m (Handler msg m h)
    flushHandle = defaultFlush

    default closeHandle :: (Monad m, Applicative m, Functor m) => Handler msg m h -> m ()
    closeHandle h = flushHandle h *> pure ()


defaultFlush h = do
    ch <- mapM flushMe $ _children h
    return $ h { _children = ch }

flushMe (HandlerWrapper h) = fmap HandlerWrapper $ flushHandle h


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

runHandler :: (Functor m, Applicative m, Monad m) => HandlerWrapper s m -> Log s -> m (HandlerWrapper s m)
runHandler (HandlerWrapper h) msg = fmap HandlerWrapper $ handle h msg

runChildren h msg = do
    let ch = view children h
    hs <- mapM (flip runHandler msg) ch
    return (h & children .~ hs)

-- Top Handler

data TopHandler = TopHandler deriving (Show)

topHandler = mkHandler TopHandler

instance (Monad m, Applicative m, Show msg) => LogHandler TopHandler m msg where
    handle = runChildren


-- Flush Handler

data FlushHandler s = FlushHandler { _buffer :: Seq (Log s) } deriving (Show)

makeLenses ''FlushHandler

enqueueMsg msg = buffer %~ (|> msg)

flushHandler = mkHandler (FlushHandler mempty)

instance (MonadIO m, Applicative m, Show msg, msg~msg') => LogHandler (FlushHandler msg) m msg' where
    handle h msg = return (h & base %~ enqueueMsg msg) -- runChildren h msg
    flushHandle h = do
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






----------------------------------------------------------------------
-- Data reading
----------------------------------------------------------------------

class LookupDataSet base s where 
    lookupDataSet :: base -> s -> Data base

instance LookupDataSet base (Data base,as) where
    lookupDataSet _ (a,_) = a

instance LookupDataSet base as => LookupDataSet base (Data b,as) where
    lookupDataSet b (_, as) = lookupDataSet b as



class Lookup base s where 
    lookup :: base -> s -> Data base

instance LookupDataSet base l => Lookup base (Log l) where
    lookup b (unLog -> s) = lookupDataSet b s

instance LookupDataSet base r => Lookup base (RecordBuilder r) where
    lookup b (fromRecordBuilder -> r) = lookupDataSet b r

--instance Lookup base (Log as) => Lookup base (Log (Data b,as)) where
--    lookup b (unLog -> (_, as)) = lookup b (Log as)


--instance Lookup base as => Lookup base (Data b,as) where
--    lookup b (_, as) = lookup b as

readData :: Lookup a l => a -> l -> DataOf a
readData a = recData . lookup a

--readData' :: (Lookup a r) => a -> Log l -> DataOf a
--readData' a = readData a

----------------------------------------------------------------------
-- Formatters
----------------------------------------------------------------------

data Formatter a = Formatter { runFormatter :: Log a -> Doc }

instance Show (Formatter a) where
    show _ = "Formatter"

--read log = readData

--foo = Formatter $ show . readData Msg

foo = colorLvlFormatter ("[" <:> Lvl <:> "] ") <:> Msg <:> " !"

colorLvlFormatter f = Formatter (\s -> let (LevelData pr _) = readData Lvl s in lvlColor pr $ runFormatter f s)

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

instance (PPrint (DataOf seg), Lookup seg (Log a)) => FormatterBuilder seg a where
    buildFormatter a = Formatter $ pprint . readData a

instance (a~b) => FormatterBuilder (Formatter a) b where
    buildFormatter = id

class PPrint a where
    pprint :: a -> Doc

instance PPrint String where
    pprint = text

instance Pretty a => PPrint a where
    pprint = pretty

instance Pretty LevelData where
    pretty (LevelData _ name) = text name

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





instance (Show s, Monad m, Applicative m) => Monoid (HandlerWrapper s m) where
    mempty = HandlerWrapper topHandler





newtype LoggerT s m a = LoggerT { unLoggerT :: StateT (HandlerWrapper s (LoggerT s m)) m a } deriving (Monad, MonadIO, Applicative, Functor)
type    Logger s a = LoggerT s Identity a

--data Logger t = Logger deriving (Show)



class (Monad m, Applicative m) => MonadLogger m where
    appendLog :: Log (LogFormat m) -> m ()
    flush     :: m ()
    close     :: m ()

    default flush :: m ()
    flush = return ()

    default close :: m ()
    close = return ()


instance (MonadTrans t, MonadLogger m, LogFormat (t m) ~ LogFormat m, Applicative m, Applicative (t m), Monad(t m)) => MonadLogger (t m) where
    appendLog = lift . appendLog


instance MonadLogger IO where
    appendLog d = putStrLn $ "IOLogger: " ++ msg where
        msg = readData Msg d

type instance LogFormat IO = (Data Msg,())


class MonadLogger m => MonadLoggerHandler h m | m -> h where
    addHandler :: h -> m ()
    -- ... ?

instance (MonadTrans t, MonadLoggerHandler h m, MonadLogger (t m)) => MonadLoggerHandler h (t m) where
    addHandler = lift . addHandler


--data Handler s m a = Handler { _base     :: a
--                             , _children :: [HandlerWrapper s m]
--                             , _level    :: Maybe Int
--                             }

--data HandlerWrapper s m = forall a. (Show a, LogHandler a m s) => HandlerWrapper (Handler s m a)



newtype RecordBuilder a = RecordBuilder { fromRecordBuilder :: a } deriving (Show, Functor)
empty = RecordBuilder ()

runLoggerT = fmap fst . flip runStateT mempty . unLoggerT
--runLogger  = runIdentity . runLoggerT

withLogState f = do
    s <- get
    put $ f s

instance (Monad m, Applicative m) => MonadLogger (LoggerT s m) where
    appendLog msg = do
        h <- get
        h' <- runHandler h msg
        put h'
    flush = do
        (HandlerWrapper s) <- get
        s' <- flushHandle s
        put (HandlerWrapper s')

instance (Monad m, Applicative m) => MonadLoggerHandler (HandlerWrapper s (LoggerT s m)) (LoggerT s m) where
    addHandler (HandlerWrapper h) = withLogState (withHandlerWrapper $ addChildHandler h)



get   = LoggerT State.get
put   = LoggerT . State.put


    --flushHandle = put mempty

--instance (Functor m, Monad m) => Flush (LoggerT s m) where
--    flushHandle = do -- put mempty
--        --hs <- getHandlers
--        return ()
--        --mapM_ flushHandle hs
--        put mempty

--appendMsg a = withLogState (msgs %~ (<> return a))

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

appData :: (a~DataOf base) => base -> a -> RecordBuilder as -> RecordBuilder (Data base, as)
appData base a = fmap (Data base a,)

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
    buildLog b = fmap Log $ (,) <$> getData <*> (unLog <$> buildLog b)


type LogBuilder' a m = LogBuilder a m (LogFormat m)


type family LogFormat (m :: * -> *)
type instance LogFormat (LoggerT log m) = log

buildLog' :: (Monad m, Applicative m, LogBuilder' a m) => RecordBuilder a -> m (Log (LogFormat m))
buildLog' = buildLog


--log :: (MonadLogger m,
--      LogBuilder (Data Lvl, (Data Msg, ())) m (LogFormat m),
--      Applicative m, Show a, Enum a) =>
--     a -> String -> m ()



--poprawic filtracje - obecnie linijka 466 jest zhardcodowana


debug     = simpleLog DEBUG
info      = simpleLog INFO
notice    = simpleLog NOTICE
warning   = simpleLog WARNING
error     = simpleLog ERROR
critical  = simpleLog CRITICAL
alert     = simpleLog ALERT
panic     = simpleLog PANIC



--flushM = flushMe =<< get



simpleLog = log empty

log d pri msg = do
-- #ifdef NOLOGS
    return ()
-- #else
    mkLog $ appData Lvl (mkLevel pri)
          $ appData Msg msg
          $ d
-- #endif



class LogConstructor d m where
    mkLog :: MonadLogger m => RecordBuilder d -> m ()

instance (LogBuilder' d m) => LogConstructor d m where
    mkLog d = do
        log <- buildLog' d
        appendLog log




-------------------------------------------------------------------------


newtype PriorityLoggerT pri m a = PriorityLoggerT { fromPriorityLoggerT :: StateT pri m a } deriving (Monad, MonadIO, Applicative, Functor, MonadTrans)

type instance LogFormat (PriorityLoggerT pri m) = LogFormat m


runPriorityLoggerT pri = fmap fst . flip runStateT pri . fromPriorityLoggerT


getPriority = PriorityLoggerT State.get

setPriority :: Monad m => pri -> PriorityLoggerT pri m ()
setPriority = PriorityLoggerT . State.put



instance (MonadLogger m, LogConstructor d m, LookupDataSet Lvl d, Enum pri) => LogConstructor d (PriorityLoggerT pri m) where
    mkLog d = do
        priLimit <- getPriority
        let LevelData pri _ = readData Lvl d
        if (fromEnum priLimit) < pri then lift $ mkLog d
                                     else return ()

-------------------------------------------------------------------------





type StdLogger m a = LoggerX (Lvl, Msg) m a

type StdTypes s = Insert Lvl (Insert Msg s) 

type LoggerX s m a = LoggerT (MapRTuple Data (StdTypes (Tuple2RTuple s))) m a

--type StdLogger2 m a = LoggerX2 (Lvl, Msg) m a

--type LoggerX2 s m a = WriterLogger (RTuple2Tuple (MapRTuple Data (StdTypes (Tuple2RTuple s)))) m a
--type LoggerX2 s m a = WriterLogger (MapRTuple Data (StdTypes s)) m a

--test2 = do
--    debug "subrutine"

test = do
    --let h = flushHandler `addChildHandler` printHandler
    addHandler (HandlerWrapper printHandler)
    debug "running subrutine"

    --x <- runWriterLoggerT $ runDupLogger test2

    --liftIO $ print x

    addHandler (HandlerWrapper printHandler)

    debug "debug1"
    setPriority DEBUG

    debug "debug2"
    info "info"
    warning "warning"

    --fail "oh no"

    flush
    liftIO $ print "---"

    critical "ola"

    --mkLog (0::Int) "hello"

    return ()


--instance Num Level

main = do
    print =<< (runLoggerT ( (runPriorityLoggerT WARNING test) :: StdLogger IO () ))
    --print =<< (runPriorityLoggerT 1 test)
    --print =<< (runLoggerT (test :: StdLogger IO () ))

    --print =<< (fmap snd $ runWriterLoggerT (test2 :: StdLogger2 IO () ))
    return ()


        --newtype DupLogger m a = DupLogger { unDupLogger :: m a } deriving (Monad, MonadIO, Applicative, Functor)

        --instance (MonadTrans t, Monad (t m), MonadLogger log (t m), MonadLogger log m) => MonadLogger log (DupLogger (t m)) where
        --    appendLog log = DupLogger $ do
        --        appendLog log
        --        lift $ appendLog log

        --runDupLogger = unDupLogger

        --newtype WriterLogger log m a = WriterLogger { unWriterLogger :: StateT (Seq (Log log)) m a } deriving (Monad, MonadIO, Applicative, Functor, MonadTrans)

        --instance Monad m => MonadLogger log (WriterLogger log m) where
        --    appendLog log = do
        --        s <- get2
        --        put2 (s |> log)
        --    --flush = do
        --    --    s <- get2
        --    --    lift $ mapM_ appendLog $ toList s
        --    --close = flush


        --get2   = WriterLogger State.get
        --put2   = WriterLogger . State.put

        ----runWriterLoggerT :: Functor m => WriterLogger log m a -> m a
        --runWriterLoggerT = flip runStateT mempty . unWriterLogger . (>> close)

        ----class Monad m => MonadLogger log m | m -> log where
        ----    appendLog :: Log log -> m ()
        ----    flush     :: m ()
        ----    close     :: m ()

        ----    default flush :: m ()
        ----    flush = return ()

        ----    default close :: m ()
        ----    close = return ()





        --instance MonadLoggerHandler h log m => MonadLoggerHandler h log (WriterLogger log m) where
        --    addHandler = WriterLogger . lift . addHandler

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

