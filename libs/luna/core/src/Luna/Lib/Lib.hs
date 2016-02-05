---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Luna.Lib.Lib where

import           Data.Binary            (Binary)
import           Data.Version           (Version)

import           Flowbox.Prelude
import           Flowbox.System.UniPath (UniPath)
import           Luna.Syntax.Module     (Module (Module))
import           Luna.Syntax.Name.Path  (QualPath)



type Name = String

data Library a e = Library
    { _name    :: Name
    , _version :: Version
    , _path    :: UniPath
    , _ast     :: Module a e
    } deriving (Show, Read, Eq, Generic)

makeLenses ''Library

instance (Binary a, Binary e) => Binary (Library a e)

newtype ID = ID { toInt :: Int } deriving (Show, Ord, Eq)


make :: Name -> Version -> UniPath -> QualPath -> Library a e
make name' version' path' mpath = Library name' version' path' emptyModule where
    emptyModule = Module mpath []

