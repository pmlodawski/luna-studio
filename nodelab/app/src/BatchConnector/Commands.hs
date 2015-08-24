module BatchConnector.Commands where

import           Data.ByteString.Lazy.Char8  (pack)
import           Utils.PreludePlus

import qualified Data.Sequence               as S
import           Text.ProtocolBuffers        (Utf8(..), messagePut)
import qualified Generated.Proto.ProjectManager.Project.List.Request   as List
import qualified Generated.Proto.ProjectManager.Project.Create.Request as Create
import           Generated.Proto.Dep.Attributes.Attributes

import           BatchConnector.Connection

createProject :: String -> String -> WSMessage
createProject name path = WSMessage "project.create.request" $ messagePut body where
    body = Create.Request (Just $ Utf8 $ pack name) (Utf8 $ pack path) (Attributes S.empty)

listProjects :: WSMessage
listProjects = WSMessage "project.list.request" $ messagePut List.Request
