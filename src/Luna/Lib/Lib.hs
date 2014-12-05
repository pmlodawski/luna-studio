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
import           Flowbox.System.UniPath (UniPath)
import           Luna.AST.Module        (Module)
import qualified Luna.AST.Module        as Module
import qualified Luna.AST.Type          as Type
import           Luna.Graph.PropertyMap (PropertyMap)
import qualified Luna.Graph.PropertyMap as PropertyMap



data Library = Library { _name        :: String
                       , _version     :: Version
                       , _path        :: UniPath
                       , _ast         :: Module
                       , _propertyMap :: PropertyMap
                       } deriving (Show, Read)

makeLenses ''Library

newtype ID = ID { toInt :: Int }
           deriving (Show, Ord, Eq)


make :: String -> Version -> UniPath -> [String] -> Library
make name' version' path' modulePath = Library name' version' path' emptyModule PropertyMap.empty where
    emptyModule = Module.mk 0 $ Type.mkModule 1 modulePath

