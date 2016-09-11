{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}

module Empire.Env where

import           Flowbox.Prelude

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

data MonitorEnv = MonitorEnv { _script           :: FilePath
                             , _lastActivityTime :: Integer
                             , _timeout          :: Integer
                             }

makeLenses ''MonitorEnv

instance Default MonitorEnv where
    def = MonitorEnv "" 0 0
