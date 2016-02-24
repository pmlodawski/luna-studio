{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}

module Empire.Env where

import           Flowbox.Prelude

import qualified Empire.Empire   as Empire
import           Control.Concurrent.STM.TChan  (TChan, newTChan)
import           Flowbox.Bus.Data.Message      (Message)

instance Show (TChan Message) where
    show _ = "(TChan)"

data Env = Env { _empireEnv :: Empire.Env
               , _formatted :: Bool
               , _toBusChan :: TChan Message
               } deriving (Show)
makeLenses ''Env

make :: TChan Message -> Env
make = Env def True

newtype LoggerEnv = LoggerEnv { _formatLog :: Bool }
makeLenses ''LoggerEnv

instance Default LoggerEnv where
    def = LoggerEnv True
