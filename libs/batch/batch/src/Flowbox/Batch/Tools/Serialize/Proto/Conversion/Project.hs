---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Batch.Tools.Serialize.Proto.Conversion.Project where

import Data.Int (Int32)

import           Flowbox.Batch.Project.Project                       (Project (Project))
import qualified Flowbox.Batch.Project.Project                       as Project
import           Flowbox.Control.Error
import           Flowbox.Data.Convert
import           Flowbox.Prelude
import qualified Generated.Proto.Project.Project                     as Gen
import qualified Luna.DEP.Data.Serialize.Proto.Conversion.Attributes ()
import           Luna.DEP.Lib.Manager                                (LibManager)



instance ConvertPure Project.ID Int32 where
    encodeP = encodeP . Project.toInt
    decodeP = Project.ID . decodeP


instance Convert (Project.ID, Project) (Gen.Project, LibManager) where
    encode (projectID, project) = (tproject, libs) where
        Project name path libPaths libs attrs = project
        tname      = fmap encodeP name
        tpath      = encodePJ path
        tlibPaths  = encodeP libPaths
        tattrs     = encodePJ attrs
        tprojectID = encodePJ projectID
        tproject   = Gen.Project tname tpath tlibPaths tattrs tprojectID
    decode (Gen.Project tname mtpath tlibPaths mtattrs mtprojectID, libs) = do
        tpath       <- mtpath      <?> "Failed to decode Project: 'path' field is missing"
        tattrs      <- mtattrs     <?> "Failed to decode Project: 'attrs' field is missing"
        tprojectID  <- mtprojectID <?> "Failed to decode Project: 'id' field is missing"
        let name      = fmap decodeP tname
            path      = decodeP tpath
            libPaths  = decodeP tlibPaths
            attrs     = decodeP tattrs
            projectID = decodeP tprojectID
            project   = Project name path libPaths libs attrs
        return (projectID, project)

