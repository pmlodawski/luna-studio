module BatchConnector.Commands where

import           Data.ByteString.Lazy.Char8 (pack)
import           Utils.PreludePlus
import qualified Data.Sequence              as Seq
import           Text.ProtocolBuffers       (Utf8(..), messagePut)
import           BatchConnector.Connection
import           Batch.Project              as Project

import qualified Generated.Proto.ProjectManager.Project.List.Request           as ListProjects
import qualified Generated.Proto.ProjectManager.Project.Create.Request         as CreateProject
import qualified Generated.Proto.ProjectManager.Project.Library.Create.Request as CreateLibrary
import qualified Generated.Proto.ProjectManager.Project.Library.List.Request   as ListLibraries
import           Generated.Proto.Dep.Version.Version
import           Generated.Proto.Dep.Attributes.Attributes

stringToUtf8 :: String -> Utf8
stringToUtf8  = Utf8 . pack

createProject :: String -> String -> WebMessage
createProject name path = WebMessage "project.create.request" $ messagePut body where
    body = CreateProject.Request (Just $ stringToUtf8 name)
                                 (stringToUtf8 path)
                                 (Attributes Seq.empty)

listProjects :: WebMessage
listProjects  = WebMessage "project.list.request" $ messagePut ListProjects.Request

createLibrary :: String -> String -> Project -> WebMessage
createLibrary name path project = WebMessage "project.library.create.request" $ messagePut body where
    body = CreateLibrary.Request (stringToUtf8 name)
                                 (Version Seq.empty Seq.empty)
                                 (stringToUtf8 path)
                                 (project ^. Project.id)

fetchLibraries :: Project -> WebMessage
fetchLibraries project = WebMessage "project.library.list.request" $ messagePut body where
    body = ListLibraries.Request (project ^. Project.id)
