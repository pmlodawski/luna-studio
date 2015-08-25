module BatchConnector.Updates where

import           Utils.PreludePlus
import           Data.ByteString.Lazy
import qualified Data.Sequence             as Seq
import           Text.ProtocolBuffers
import           Batch.Project
import           BatchConnector.Conversion

import qualified Generated.Proto.ProjectManager.Project.List.Status   as ProjectsList
import qualified Generated.Proto.ProjectManager.Project.Create.Update as ProjectCreated

parseMessage :: (Wire m, ReflectDescriptor m) => ByteString -> Maybe m
parseMessage bytes = case messageGet bytes of
    Left _ -> Nothing
    Right (msg, _) -> Just msg

deserializeProjectsStatus :: ProjectsList.Status -> [Project]
deserializeProjectsStatus  = (fmap deserialize) . toList . ProjectsList.projects

parseProjectsList :: ByteString -> Maybe [Project]
parseProjectsList  = (fmap deserializeProjectsStatus) . parseMessage

parseProjectCreateUpdate :: ByteString -> Maybe Project
parseProjectCreateUpdate  = (fmap $ deserialize . ProjectCreated.project) . parseMessage
