{-# LANGUAGE ScopedTypeVariables       #-}
module Empire.Handlers where

import           Prologue
import           Prelude (undefined)

import           Control.Monad.State   (StateT)
import           Data.ByteString       (ByteString)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import qualified Empire.API.Topic      as Topic
import           Empire.Env            (Env)
import qualified Empire.Server.Graph   as Graph
import qualified Empire.Server.Library as Library
import qualified Empire.Server.Project as Project
import           Flowbox.Bus.BusT      (BusT (..))
import qualified Data.Binary                      as Bin
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Lazy             (fromStrict)


type Handler = ByteString -> StateT Env BusT ()

handlersMap :: Map String Handler
handlersMap = Map.fromList
    [ makeHandler Graph.handleAddNode
    , makeHandler Graph.handleRemoveNode
    , makeHandler Graph.handleUpdateNodeMeta
    , makeHandler Graph.handleRenameNode
    , makeHandler Graph.handleConnect
    , makeHandler Graph.handleDisconnect
    , makeHandler Graph.handleSetDefaultValue
    , makeHandler Graph.handleGetProgram
    , makeHandler Graph.handleDumpGraphViz
    , makeHandler Graph.handleTypecheck
    , makeHandler Project.handleCreateProject
    , makeHandler Project.handleListProjects
    , makeHandler Project.handleExportProject
    , makeHandler Project.handleImportProject
    , makeHandler Library.handleCreateLibrary
    , makeHandler Library.handleListLibraries
    ]

makeHandler :: forall a. (Topic.MessageTopic a, Bin.Binary a) => (a -> StateT Env BusT ()) -> (String, Handler)
makeHandler h = (Topic.topic (undefined :: a), process) where
   process content = h request where request = Bin.decode . fromStrict $ content
