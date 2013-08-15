---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Batch.Tools.Serialize.Thrift.Conversion.Projects where


import           Data.Text.Lazy                                              (pack, unpack)

import qualified Projects_Types                                            as TProjects
import qualified Flowbox.Batch.Project.Project                             as Project
import           Flowbox.Batch.Project.Project                               (Project(..))
import           Flowbox.Luna.Lib.LibManager                                 (LibManager)
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs         ()
import qualified Flowbox.System.UniPath                                    as UniPath
import           Flowbox.Tools.Conversion


instance Convert (Project.ID, Project) (TProjects.Project, LibManager) where
    encode (projectID, project) = (tproject, alibs) where
        Project aname apath alibs aattrs = project
        tname      = pack aname
        tpath      = pack $ UniPath.toUnixString apath
        tattrs     = encode aattrs
        tprojectID = itoi32 projectID
        tproject   = TProjects.Project (Just tname) (Just tpath) (Just tattrs) (Just tprojectID)
    decode (TProjects.Project mtname mtpath mtattrs mtprojectID, alibs) = case mtname of
        Nothing                             -> Left "`name` field is missing"
        Just tname                          -> case mtpath of
            Nothing                         -> Left "`path` field is missing"
            Just tpath                      -> case mtattrs of
                Nothing                     -> Left "`attrs` field is missing"
                Just tattrs                 -> case decode tattrs of
                    Left message            -> Left $ "Failed to decode `attrs`: " ++ message
                    Right aattrs            -> case mtprojectID of
                        Nothing             -> Left "`projectID` field is missing"
                        Just tprojectID     -> Right (projectID, project) where
                            aname      = unpack tname
                            apath      = UniPath.fromUnixString $ unpack tpath
                            projectID = i32toi tprojectID
                            project   = Project aname apath alibs aattrs

