module BatchConnector.Commands where

import           Data.ByteString.Lazy.Char8  (pack)
import           Utils.PreludePlus

import qualified Data.Sequence               as Seq
import           Text.ProtocolBuffers        (Utf8(..), messagePut)
import qualified Generated.Proto.ProjectManager.Project.List.Request   as List
import qualified Generated.Proto.ProjectManager.Project.Create.Request as Create
import           Generated.Proto.Dep.Attributes.Attributes

import           BatchConnector.Connection

createProject :: String -> String -> WebMessage
createProject name path = WebMessage "project.create.request" $ messagePut body where
    body         = Create.Request (Just $ stringToUtf8 name) (stringToUtf8 path) (Attributes Seq.empty)
    stringToUtf8 = Utf8 . pack

listProjects :: WebMessage
listProjects  = WebMessage "project.list.request" $ messagePut List.Request
