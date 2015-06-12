-----------------------------------------------------------------------------
-- |
-- Module      :  Luna.System.Config
-- Copyright   :  (C) 2014 Flowbox
-- License     :  AllRightsReserved
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
module Luna.System.Config where

import Flowbox.Prelude

import           Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import qualified System.Environment  as Env

data Env     = Env     { _path :: String }
                       deriving (Show)

data Sources = Sources { _sourceExt  :: String
                       , _modInfoExt :: String
                       } deriving (Show)

data Config  = Config  { _env    :: Env 
                       , _souces :: Sources
                       } deriving (Show)
makeLenses ''Env
makeLenses ''Sources
makeLenses ''Config

class ConfigMonad m where
    get :: m Config
    put :: Config -> m ()

-- == Utils ==

readPath :: (MonadIO m, ConfigMonad m, Functor m) => m String
readPath = do
    pathName <- view (env.path) <$> get
    envPath  <- liftIO $ Env.lookupEnv pathName
    return $ maybe "" id envPath


-- == Instances ==

instance Default Env where
    def = Env { _path = "LUNAPATH" }

instance Default Sources where
    def = Sources { _sourceExt  = "luna" 
                  , _modInfoExt = "li"
                  } 

instance Default Config where
    def = Config def def

instance Monad m => ConfigMonad (StateT Config m) where
    get = State.get
    put = State.put