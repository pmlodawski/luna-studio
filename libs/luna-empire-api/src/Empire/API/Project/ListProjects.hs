{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Empire.API.Project.ListProjects where

import           Prologue
import           Data.Binary             (Binary)

import           Empire.API.Data.Project (ProjectId, Project)
import qualified Empire.API.Response             as Response
import qualified Empire.API.Topic              as T

data Request = Request deriving (Generic, Show, Eq)

data Result  = Result { _projects :: [(ProjectId, Project)]
                      } deriving (Generic, Show, Eq)

type Response = Response.Response Request Result
instance Response.ResponseResult Request Result

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance Binary Result
                              
topicPrefix = "empire.project.list"
instance T.MessageTopic Request  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
