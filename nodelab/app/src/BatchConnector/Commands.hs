module BatchConnector.Commands where

import           Data.ByteString.Lazy.Char8 (pack)
import           Utils.PreludePlus
import qualified Data.Sequence              as Seq
import           Text.ProtocolBuffers       (Utf8(..), messagePut)
import           Text.ProtocolBuffers.Basic (uFromString)
import           BatchConnector.Connection
import           Batch.Project              as Project
import           Batch.Library              as Library
import           Batch.Breadcrumbs
import           Data.Map                   as Map
import           BatchConnector.Conversion

import           Batch.Function

import qualified Generated.Proto.ProjectManager.Project.List.Request                     as ListProjects
import qualified Generated.Proto.ProjectManager.Project.Create.Request                   as CreateProject
import qualified Generated.Proto.ProjectManager.Project.Library.Create.Request           as CreateLibrary
import qualified Generated.Proto.ProjectManager.Project.Library.List.Request             as ListLibraries
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Add.Request as AddFunction
import           Generated.Proto.Dep.Version.Version
import           Generated.Proto.Dep.Attributes.Attributes
import           Generated.Proto.Dep.Module.Module
import qualified Generated.Proto.Dep.Crumb.Breadcrumbs                        as ProtoBreadcrumbs
import qualified Generated.Proto.Interpreter.Interpreter.SetProjectID.Request as SetProjectId
import qualified Generated.Proto.Interpreter.Interpreter.Run.Request          as Run
import qualified Generated.Proto.Interpreter.Interpreter.SetMainPtr.Request   as SetMainPtr
import qualified Generated.Proto.Interpreter.DefPoint                         as DefPoint

createProject :: String -> String -> WebMessage
createProject name path = WebMessage "project.create.request" $ messagePut body where
    body = CreateProject.Request (Just $ uFromString name)
                                 (uFromString path)
                                 (Attributes Seq.empty)

listProjects :: WebMessage
listProjects  = WebMessage "project.list.request" $ messagePut ListProjects.Request

createLibrary :: String -> String -> Project -> WebMessage
createLibrary name path project = WebMessage "project.library.create.request" $ messagePut body where
    body = CreateLibrary.Request (uFromString name)
                                 (Version Seq.empty Seq.empty)
                                 (uFromString path)
                                 (project ^. Project.id)

fetchLibraries :: Project -> WebMessage
fetchLibraries project = WebMessage "project.library.list.request" $ messagePut body where
    body = ListLibraries.Request (project ^. Project.id)

setProjectId :: Project -> WebMessage
setProjectId project = WebMessage "interpreter.setprojectid.request" $ messagePut body where
    body = SetProjectId.Request (project ^. Project.id)

createMainFunction :: Project -> Library -> WebMessage
createMainFunction project library = WebMessage "project.library.ast.function.add.request" $ messagePut body where
    body = AddFunction.Request emptyFunctionExpr
                               (ProtoBreadcrumbs.Breadcrumbs $ Seq.fromList [moduleCrumb "Main"])
                               (library ^. Library.id)
                               (project ^. Project.id)
                               1

runMain :: WebMessage
runMain  = WebMessage "interpreter.run.request" $ messagePut $ Run.Request Nothing

setMainPtr :: Project -> Library -> Breadcrumbs -> WebMessage
setMainPtr proj lib crumbs = WebMessage "interpreter.setmainptr.request" $ messagePut body where
    body     = SetMainPtr.Request defPoint
    defPoint = DefPoint.DefPoint (proj ^. Project.id)
                                 (lib  ^. Library.id)
                                 (encode crumbs)
