{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}

module Empire.Env where

import           Flowbox.Prelude

import           Control.Concurrent.STM.TChan      (TChan)
import           Data.Map.Lazy                     (Map)
import           Data.UUID                         (UUID)
import qualified Data.UUID                         as UUID (nil)
import           Empire.API.Data.AsyncUpdate       (AsyncUpdate)
import qualified Empire.API.Data.DefaultValue      as DefaultValue
import           Empire.API.Data.GraphLocation     (GraphLocation(..))
import           Empire.API.Data.Breadcrumb        (Breadcrumb(..))
import           Empire.API.Data.Node              (Node, NodeId)
import           Empire.API.Data.Project           (ProjectId)
import qualified Empire.API.Graph.GetProgram       as GetProgram
import qualified Empire.API.Graph.NodeResultUpdate as NodeResultUpdate
import           Empire.Data.Graph                 (Graph)
import qualified Empire.Empire                     as Empire
import           Flowbox.Bus.Data.Message          (Message)

instance Show (TChan Message) where
    show _ = "(TChan)"

data Env = Env { _empireEnv   :: Empire.Env
               , _empireNotif :: Empire.CommunicationEnv
               , _formatted   :: Bool
               , _toBusChan   :: TChan Message
               , _projectRoot :: FilePath
               } deriving (Show)
makeLenses ''Env

make :: TChan Message -> TChan AsyncUpdate -> TChan (GraphLocation, Graph, Bool) -> FilePath -> Env
make toBus fromEmpire tc = Env def (Empire.CommunicationEnv fromEmpire tc) True toBus

newtype LoggerEnv = LoggerEnv { _formatLog :: Bool }
makeLenses ''LoggerEnv

instance Default LoggerEnv where
    def = LoggerEnv True
