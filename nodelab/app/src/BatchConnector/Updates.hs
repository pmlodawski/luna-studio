module BatchConnector.Updates where

import           Utils.PreludePlus
import           Data.ByteString.Lazy
import qualified Data.Sequence             as Seq
import           Text.ProtocolBuffers
import           Batch.Project
import           Batch.Library
import           BatchConnector.Conversion (decode)

import qualified Generated.Proto.ProjectManager.Project.List.Status           as ProjectsList
import qualified Generated.Proto.ProjectManager.Project.Create.Update         as ProjectCreated
import qualified Generated.Proto.ProjectManager.Project.Library.List.Status   as LibsList
import qualified Generated.Proto.ProjectManager.Project.Library.Create.Update as LibCreated

parseMessage :: (Wire m, ReflectDescriptor m) => ByteString -> Maybe m
parseMessage bytes = case messageGet bytes of
    Left  _        -> Nothing
    Right (msg, _) -> Just msg

pluckProjects :: ProjectsList.Status -> Maybe [Project]
pluckProjects  = decode . ProjectsList.projects

parseProjectsList :: ByteString -> Maybe [Project]
parseProjectsList bytes = (parseMessage bytes) >>= pluckProjects

parseProjectCreateUpdate :: ByteString -> Maybe Project
parseProjectCreateUpdate bytes = (parseMessage bytes) >>= getProject where
    getProject = decode . ProjectCreated.project

parseLibrariesListResponse :: ByteString -> Maybe [Library]
parseLibrariesListResponse bytes = (parseMessage bytes) >>= getLibs where
    getLibs = decode . LibsList.libraries

parseLibraryCreateResponse :: ByteString -> Maybe Library
parseLibraryCreateResponse bytes = (parseMessage bytes) >>= getLib where
    getLib = decode . LibCreated.library
