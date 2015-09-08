---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Luna.DEP.Lib.Lib where

import Data.Version (Version)

import           Flowbox.Prelude
import           Flowbox.System.UniPath     (UniPath)
import           Luna.DEP.AST.Module        (Module)
import qualified Luna.DEP.AST.Module        as Module
import qualified Luna.DEP.AST.Type          as Type
import           Luna.DEP.Data.ASTInfo      (ASTInfo (ASTInfo))
import           Luna.DEP.Graph.PropertyMap (PropertyMap)


type Name = String

data Library = Library { _name        :: Name
                       , _version     :: Version
                       , _path        :: UniPath
                       , _ast         :: Module
                       , _propertyMap :: PropertyMap
                       , _astInfo     :: ASTInfo
                       } deriving (Show, Read, Eq)

makeLenses ''Library

newtype ID = ID { toInt :: Int }
           deriving (Show, Ord, Eq)


make :: Name -> Version -> UniPath -> [String] -> Library
make name' version' path' modulePath = Library name' version' path' emptyModule def $ ASTInfo 1 where
    emptyModule = Module.mk 0 $ Type.mkModule 1 modulePath

