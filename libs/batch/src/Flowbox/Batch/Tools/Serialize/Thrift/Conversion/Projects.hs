---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Batch.Tools.Serialize.Thrift.Conversion.Projects where

import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict   (HashMap)
import           Data.Text.Lazy        (Text, pack, unpack)

import qualified Projects_Types                           as TProjects
import qualified Flowbox.Batch.Project.Project            as Project
import           Flowbox.Batch.Project.Project              (Project(..))
import           Flowbox.Luna.Core                          (Core(..))
import           Flowbox.Luna.Network.Flags                 (Flags(..))
import qualified Flowbox.Luna.Network.Attributes          as Attributes
import           Flowbox.Luna.Network.Attributes            (Attributes)
import qualified Flowbox.System.UniPath                   as UniPath
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs         ()



instance Convert (Project.ID, Project) (TProjects.Project, Core) where
    encode (projectID, project) = (tproject, core) where
        Project name path core attrs = project
        tname      = pack name
        tpath      = pack $ UniPath.toUnixString path
        tattrs     = encode attrs
        tprojectID = itoi32 projectID
        tproject   = TProjects.Project (Just tname) (Just tpath) (Just tattrs) (Just tprojectID)
    decode (TProjects.Project mtname mtpath mtattrs mtprojectID, core) = case mtname of
        Nothing                             -> Left "`name` field is missing"
        Just tname                          -> case mtpath of
            Nothing                         -> Left "`path` field is missing"
            Just tpath                      -> case mtattrs of
                Nothing                     -> Left "`attrs` field is missing"
                Just tattrs                 -> case decode tattrs of
                    Left message            -> Left $ "Failed to decode `attrs`: " ++ message
                    Right attrs             -> case mtprojectID of
                        Nothing             -> Left "`projectID` field is missing"
                        Just tprojectID     -> Right (projectID, project) where
                            name      = unpack tname
                            path      = UniPath.fromUnixString $ unpack tpath
                            projectID = i32toi tprojectID
                            project   = Project name path core attrs

