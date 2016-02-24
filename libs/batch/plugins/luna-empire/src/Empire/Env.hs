{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}

module Empire.Env where

import           Flowbox.Prelude

import qualified Empire.Empire                 as Empire
import           Empire.API.Data.AsyncUpdate   (AsyncUpdate)
import           Control.Concurrent.STM.TChan  (TChan, newTChan)
import           Flowbox.Bus.Data.Message      (Message)

instance Show (TChan Message) where
    show _ = "(TChan)"

data Env = Env { _empireEnv   :: Empire.Env
               , _empireNotif :: Empire.NotifierEnv
               , _formatted   :: Bool
               , _toBusChan   :: TChan Message
               } deriving (Show)
makeLenses ''Env

make :: TChan Message -> TChan AsyncUpdate -> Env
make toBus fromEmpire = Env def (Empire.NotifierEnv fromEmpire) True toBus

newtype LoggerEnv = LoggerEnv { _formatLog :: Bool }
makeLenses ''LoggerEnv

instance Default LoggerEnv where
    def = LoggerEnv True
