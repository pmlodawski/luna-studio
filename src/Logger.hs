module Logger (
  LoggerT, Logger,
  function, functionResult, trace, err,
  formatStack, runLogger, evalLoggerT
    ) where

import Control.Applicative (Applicative(..),Alternative(..))
import Control.Monad (MonadPlus(..),ap,liftM,join)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans (MonadTrans(..))
import Data.Functor.Identity (Identity,runIdentity)
import Data.Maybe (isJust)


data Log = CallResult String (Maybe String) [Log]
         | Call       String Bool           [Log]
         | Log        String
         | Error      String

instance Show Log where
  showList = (++) . formatStack False
  show x = formatStack True [x]

newtype LoggerT m a = LoggerT { runLoggerT :: m (Maybe a, [Log]) }
type Logger = LoggerT Identity


instance (Monad m) => Monad (LoggerT m) where
  return x = LoggerT $ return (Just x, [])
  m >>= f = LoggerT $ do
    v <- runLoggerT m
    case v of
      (Nothing, s) -> return (Nothing, s)
      (Just a, s) -> do (mb, s') <- runLoggerT (f a)
                        return (mb, s++s')

instance (Monad m) => Alternative (LoggerT m) where
  empty = LoggerT $ return (Nothing, [])
  (<|>) = (>>)

instance (Monad m) => MonadPlus (LoggerT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans LoggerT where
  lift ma = LoggerT $ do
    res <- ma
    return (Just res, [])

instance (MonadIO m) => MonadIO (LoggerT m) where
  liftIO mio = LoggerT $ do
    res <- liftIO mio
    return (Just res, [])

instance (Monad m) => Functor (LoggerT m) where
  fmap = liftM

instance (Monad m) => Applicative (LoggerT m) where
  pure = return
  (<*>) = ap


evalLoggerT :: Monad m => LoggerT m a -> m (Maybe a)
evalLoggerT logger = runLoggerT logger >>= return . fst

runLogger :: Logger a -> (Maybe a, [Log])
runLogger = runIdentity . runLoggerT 

formatStack :: Bool -> [Log] -> String
formatStack fulldebug = concatMap (printStack 0)
  where printStack n (CallResult str (Just a) stck2) = concat
          [ indent n
          , "CALL: "++str++" … -> "++show a++"\n"
          , if fulldebug then (concatMap (printStack $ n+1) stck2) else ""
          ]
        printStack n (CallResult str Nothing stck2) = concat
          [ indent n
          , "CALL: "
          , str
          , " …\n"
          , concatMap (printStack $ n+1) stck2
          ]
        printStack n (Call str True stck2) = concat
          [ indent n
          , "CALL: "
          , str
          , "\n"
          , if fulldebug then concatMap (printStack $ n+1) stck2 else ""
          ]
        printStack n (Call str False stck2) = concat
          [ indent n
          , "CALL: "
          , str
          , "\n"
          , concatMap (printStack $ n+1) stck2
          ]
        printStack n (Log str) = concat
          [ indent n
          , "LOG : "
          , str
          , "\n"
          ]
        printStack n (Error str) = concat
          [ indent n
          , "ERR : "
          , str
          , "\n"
          ]
        indent n = if (n>0)
                 then join (replicate (n-1) "    ") ++ "  - "
                 else ""

--showoffLogger :: Show r => Bool -> LoggerT IO r -> IO ()
--showoffLogger fulldebug logger = do
--  (mres, s) <- runLoggerT logger
--  putStrLn $ maybe "error occured, no result available"
--                   (("final result is "++) . show)
--                   mres
--  putStrLn $ formatStack fulldebug s

function :: (Monad m) => String -> LoggerT m r -> LoggerT m r
function str mm = LoggerT $ do
  (ma, s) <- runLoggerT mm
  return (ma, [Call str (isJust ma) s])

functionResult :: (Monad m, Show r) => String -> LoggerT m r -> LoggerT m r
functionResult str mm = LoggerT $ do
  (ma, s) <- runLoggerT mm
  return (ma, [CallResult str (fmap show ma) s])

trace :: Monad m => String -> LoggerT m ()
trace s = LoggerT $ return (Just (), [Log s])

err :: Monad m => String -> LoggerT m ()
err s = LoggerT $ return (Nothing, [Error s])





--main = do
--  --let (mres, s) = runIdentity $ runLoggerT test1
--  --putStrLn $ formatStack True s
--  (mres', s') <- runLoggerT test1
--  putStrLn $ formatStack True s'


--test1 = function "test1" $ do
--  liftIO $ print "DUPA!!!"
--  trace "przed startem test2"
--  x <- test2 100
--  trace "po starcie test2"
--  trace "startuję test2 drugi raz!"
--  y <- test2 200
--  trace "udało się!"
--  trace "to tereaz może test3?"
--  z <- test3
--  trace "nie powinno było się udać ;_;"
--  liftIO $ print "DUPA2!!1"
--  return (x+y+z)

--test2 i = function "test2" $ do
--  trace $ "test2 z argumentem " ++ show i
--  return (i + 1000)

--test3 = function "test3" $ do
--  trace "przed startem test4"
--  x <- test4 100
--  trace "po starcie test4"
--  return x

--test4 i = function "test4" $ do
--  err $ "błąd gdyż ponieważ " ++ show i
--  return (i - 1000)
