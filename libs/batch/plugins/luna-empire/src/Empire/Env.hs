{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}

module Empire.Env where

import           Flowbox.Prelude

import qualified Empire.Empire                 as Empire
import           Empire.API.Data.AsyncUpdate   (AsyncUpdate)
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.Data.Graph             (Graph)
import           Control.Concurrent.STM.TChan  (TChan, newTChan)
import           Flowbox.Bus.Data.Message      (Message)

instance Show (TChan Message) where
    show _ = "(TChan)"

data Env = Env { _empireEnv   :: Empire.Env
               , _empireNotif :: Empire.CommunicationEnv
               , _formatted   :: Bool
               , _toBusChan   :: TChan Message
               } deriving (Show)
makeLenses ''Env

make :: TChan Message -> TChan AsyncUpdate -> TChan (GraphLocation, Graph) -> Env
make toBus fromEmpire tc = Env def (Empire.CommunicationEnv fromEmpire tc) True toBus

newtype LoggerEnv = LoggerEnv { _formatLog :: Bool }
makeLenses ''LoggerEnv

instance Default LoggerEnv where
    def = LoggerEnv True
