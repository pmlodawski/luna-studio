---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Batch.Tools.Serialize.Proto.Conversion.Project where

import qualified Flowbox.Luna.Tools.Serialize.Proto.Conversion.Attributes   ()

import           Flowbox.Prelude                                            
import qualified Flowbox.Batch.Project.Project                            as Project
import           Flowbox.Batch.Project.Project                              (Project(..))
import           Flowbox.Control.Error                                      
import           Flowbox.Luna.Lib.LibManager                                (LibManager)
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic             
import qualified Generated.Proto.Project.Project                          as Gen



instance Convert (Project.ID, Project) (Gen.Project, LibManager) where
    encode (projectID, project) = (tproject, alibs) where
        Project aname apath alibPaths alibs aattrs = project
        tname      = encodeP aname
        tpath      = encodeP apath
        tlibPaths  = encodeListP alibPaths
        tattrs     = encode  aattrs
        tprojectID = encodeP projectID
        tproject   = Gen.Project (Just tname) (Just tpath) tlibPaths (Just tattrs) (Just tprojectID)
    decode (Gen.Project mtname mtpath tlibPaths mtattrs mtprojectID, alibs) = do
        tname       <- mtname      <?> "Failed to decode Project: 'name' field is missing"
        tpath       <- mtpath      <?> "Failed to decode Project: 'path' field is missing"
        tattrs      <- mtattrs     <?> "Failed to decode Project: 'attrs' field is missing"
        tprojectID  <- mtprojectID <?> "Failed to decode Project: 'projectID' field is missing"
        aattrs      <- decode tattrs
        let aname     = decodeP tname
            apath     = decodeP tpath
            alibPaths = decodeListP tlibPaths
            projectID = decodeP tprojectID
            project   = Project aname apath alibPaths alibs aattrs
        return (projectID, project)

