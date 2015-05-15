---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Flowbox.Batch.Project.Project where

import Data.Int     (Int32)
import GHC.Generics ()

import           Flowbox.Data.Convert
import           Flowbox.Prelude
import           Flowbox.System.UniPath                              (UniPath)
import qualified Generated.Proto.Project.Project                     as Gen
import qualified Luna.DEP.Data.Serialize.Proto.Conversion.Attributes ()
import qualified Luna.DEP.Data.Serialize.Proto.Conversion.Library    ()
import           Luna.DEP.Graph.Attributes                           (Attributes)
import           Luna.DEP.Lib.Manager                                (LibManager)



data Project = Project { _name     :: Maybe String
                       , _path     :: UniPath
                       , _libPaths :: [UniPath]
                       , _libs     :: LibManager
                       , _attrs    :: Attributes
                       } deriving (Show, Read)

makeLenses ''Project


newtype ID = ID { toInt :: Int }
           deriving (Show, Ord, Eq, Generic)


instance Default Project where
    def = Project Nothing def def def def


make :: Maybe String -> UniPath -> Attributes -> Project
make name' path' attrs' = def & name  .~ name'
                              & path  .~ path'
                              & attrs .~ attrs'

-- protocol buffers

instance ConvertPure ID Int32 where
    encodeP = encodeP . toInt
    decodeP = ID . decodeP


instance Convert (ID, Project) Gen.Project where
    encode (projectID, project) = tproject where
        Project name' path' libPaths' libs' attrs' = project
        tname      = fmap encodeP name'
        tpath      = encodeP path'
        tlibPaths  = encodeP libPaths'
        tlibs      = encode  libs'
        tattrs     = encodeP attrs'
        tprojectID = encodeP projectID
        tproject   = Gen.Project tname tpath tlibPaths tlibs tattrs tprojectID
    decode (Gen.Project tname tpath tlibPaths tlibs tattrs tprojectID) = do
        libs'       <- decode tlibs
        let name'      = fmap decodeP tname
            path'      = decodeP tpath
            libPaths'  = decodeP tlibPaths
            attrs'     = decodeP tattrs
            projectID  = decodeP tprojectID
            project    = Project name' path' libPaths' libs' attrs'
        return (projectID, project)
