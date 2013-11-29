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
import           Flowbox.Batch.Project.Project                              (Project(Project))
import           Flowbox.Control.Error                                      
import           Flowbox.Luna.Lib.LibManager                                (LibManager)
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic             
import qualified Generated.Proto.Project.Project                          as Gen



instance Convert (Project.ID, Project) (Gen.Project, LibManager) where
    encode (projectID, project) = (tproject, libs) where
        Project name path libPaths libs attrs = project
        tname      = encodePJ name
        tpath      = encodePJ path
        tlibPaths  = encodeListP libPaths
        tattrs     = encodePJ attrs
        tprojectID = encodePJ projectID
        tproject   = Gen.Project tname tpath tlibPaths tattrs tprojectID
    decode (Gen.Project mtname mtpath tlibPaths mtattrs mtprojectID, libs) = do
        tname       <- mtname      <?> "Failed to decode Project: 'name' field is missing"
        tpath       <- mtpath      <?> "Failed to decode Project: 'path' field is missing"
        tattrs      <- mtattrs     <?> "Failed to decode Project: 'attrs' field is missing"
        tprojectID  <- mtprojectID <?> "Failed to decode Project: 'projectID' field is missing"
        let name      = decodeP tname
            path      = decodeP tpath
            libPaths  = decodeListP tlibPaths
            attrs     = decodeP tattrs
            projectID = decodeP tprojectID
            project   = Project name path libPaths libs attrs
        return (projectID, project)

