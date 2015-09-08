---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE DeriveGeneric         #-}

module Flowbox.ProjectManager.RPC.Handler.Project2 where

import           Data.ByteString.Lazy (ByteString)
import           Data.Binary     
import           GHC.Generics    ()

import           Flowbox.Batch.Batch                                   (Batch)
import qualified Flowbox.Batch.Batch                                   as Batch
import qualified Flowbox.Batch.Handler.Common         as Batch
import qualified Flowbox.Batch.Project.Project                         as Project
import qualified Flowbox.Batch.Project.ProjectManager as ProjectManager
import           Flowbox.Bus.RPC.RPC                                   (RPC)
import           Flowbox.Prelude                 hiding (Context, Traversal, cons, drop, id)
import           Flowbox.ProjectManager.Context                        (Context)


type FunctionName = String


data Request = Request { requestMethod :: FunctionName
		       , arguments     :: Value
		       }

data Response = Response { responseMethod ::FunctionName
	                 , result         ::Result
	                 , messages       ::[Value] 
	                 }

data Result = ErrorResult  { errorMsg :: String }
	    | Status 	   { retVal   :: Value  }


data Value = Value { typename  :: String
		   , protocol  :: String
		   , dataBytes :: ByteString 
		   }

data ClearStackRequest = ClearStackRequest { _clearPojectID :: Project.ID }
                                     deriving (Show, Generic)

data UpdateProjectClosed = UpdateProjectClosed { _updateProjectID :: Project.ID }
                                     deriving (Show, Generic)

instance Binary ClearStackRequest
instance Binary UpdateProjectClosed
instance Binary Project.ID

makeLenses ''ClearStackRequest
makeLenses ''UpdateProjectClosed

deserialize :: Binary a => Value -> a
deserialize (Value _ "bin" bytes) = decode bytes

asd :: [Value] -> (Result, [Value])
asd = (,) <$> (Status . head) <*> tail

--packValues = map (flip Value "bin" <$> show . ' <*> encode)


--list' :: Request -> RPC Context IO (Result, [Value])
--list' _ = Project.listElem >>= return . encode . asd fname
--
--
--listElem :: Batch ([Value])
--listElem = do
--    projects <- ProjectManager.labNodes <$> Batch.getProjectManager
--    let tprojects       = map (encodeProject . set (_2 . Project.libs) def) projects
--        tprojectsVector = Sequence.fromList tprojects
--    return [tprojectsVector]


closeProject' :: Request -> RPC Context IO (Result, [Value])
closeProject' (Request _ argValue) = 
    closeProjectElem arg1 >>= return . asd
    where (arg1) = deserialize argValue :: (Project.ID)

closeProjectElem :: Project.ID -> Batch ([Value])
closeProjectElem id = do
    closeProjectApply update
    return [updateValue, undoredoInfo]
    where update       = UpdateProjectClosed id
          updateValue  = Value (show 'UpdateProjectClosed) "bin" $ encode update
          undoredoInfo = Value (show 'ClearStackRequest  ) "bin" $ encode $ ClearStackRequest   id

closeProjectApply :: UpdateProjectClosed -> Batch ()
closeProjectApply (UpdateProjectClosed id) = do
    Batch.put =<< (Batch.idMap .~ Batch.emptyIDMap) <$> Batch.get
    Batch.projectManagerOp $ \projectManager ->
        return (ProjectManager.delNode id projectManager, ())
