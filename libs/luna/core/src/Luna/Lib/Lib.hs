---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Luna.Lib.Lib where

import Data.Version (Version)

import           Flowbox.Prelude
import           Flowbox.System.UniPath        (UniPath)
import           Luna.Syntax.Graph.PropertyMap (PropertyMap)
import           Luna.Syntax.Module            (Module (Module))
import           Luna.Syntax.Name.Path         (QualPath)



data Library a e v = Library { _name        :: String
                             , _version     :: Version
                             , _path        :: UniPath
                             , _ast         :: Module a e
                             , _propertyMap :: PropertyMap a v
                             } deriving (Show, Read, Eq)

makeLenses ''Library

newtype ID = ID { toInt :: Int }
           deriving (Show, Ord, Eq)


make :: String -> Version -> UniPath -> QualPath -> Library a e v
make name' version' path' mpath = Library name' version' path' emptyModule def where
    emptyModule = Module mpath []

