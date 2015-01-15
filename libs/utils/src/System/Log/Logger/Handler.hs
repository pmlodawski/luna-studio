{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Log.Logger.Handler
-- Copyright   :  (C) 2015 Flowbox
-- License     :  Apache-2.0
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module System.Log.Logger.Handler where

import           Data.Monoid
import           Control.Applicative
import           System.Log.Data               (MonadRecord(appendRecord), LogBuilder)
import           Control.Lens                  hiding (children)
import           System.Log.Log                (Log, MonadLogger(appendLog), LogFormat, LogFormat)
import           Control.Monad.Trans           (lift)
import           Control.Monad.State           (StateT, runStateT)
import qualified Control.Monad.State           as State
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           System.Log.Format             (runFormatter, foo)
import           Text.PrettyPrint.ANSI.Leijen  (putDoc)
import Control.Monad.Trans (MonadTrans)


----------------------------------------------------------------------
-- MonadLoggerHandler
----------------------------------------------------------------------

class MonadLoggerHandler h m | m -> h where
    addHandler :: h -> m ()

    default addHandler :: (MonadTrans t, Monad m) => h -> (t m) ()
    addHandler = lift . addHandler

----------------------------------------------------------------------
-- Handler
----------------------------------------------------------------------

-- !!! dorobic formattery i filtracje do handlerow!

data Handler m l = Handler { _name     :: String
                           , _action   :: Log l -> m ()
                           , _children :: [Handler m l]
                           , _level    :: Maybe Int
                           }
makeLenses ''Handler

instance Show (Handler m l) where
    show (Handler n _ _ pr) = "Handler " <> n <> " " <> show pr

mkHandler :: String -> (Log l -> m ()) -> Handler m l
mkHandler name f = Handler name f [] Nothing

-- === Handlers ===

topHandler   = mkHandler "TopHandler" (\_ -> return ())

printHandler = mkHandler "PrintHandler" $ handle2

handle2 l = do
        liftIO $ putDoc $ runFormatter foo l
        liftIO $ putStrLn ""


addChildHandler h ph = ph & children %~ (h:)

----------------------------------------------------------------------
-- HandlerLogger
----------------------------------------------------------------------

newtype HandlerLogger m a = HandlerLogger { fromHandlerLogger :: StateT (Handler (HandlerLogger m) (LogFormat m)) m a } deriving (Monad, MonadIO, Applicative, Functor)

type instance LogFormat (HandlerLogger m) = LogFormat m

runHandlerLoggerT :: (Functor m, Monad m) => HandlerLogger m b -> m b
runHandlerLoggerT = fmap fst . flip runStateT topHandler . fromHandlerLogger

runHandler :: (Applicative m, Monad m) => Log l -> Handler m l -> m ()
runHandler l h = (h^.action) l <* mapM (runHandler l) (h^.children)

getTopHandler = HandlerLogger State.get
putTopHandler = HandlerLogger . State.put

-- === Instances ===

instance (Monad m, Functor m) => MonadLogger (HandlerLogger m) where
    appendLog l =  (runHandler l =<< getTopHandler)

instance (Monad m, Functor m, l~LogFormat m) => MonadLoggerHandler (Handler (HandlerLogger m) l) (HandlerLogger m) where
    addHandler h = do
        topH <- getTopHandler
        putTopHandler $ addChildHandler h topH

instance (Functor m, Monad m, LogBuilder d (HandlerLogger m)) => MonadRecord d (HandlerLogger m)