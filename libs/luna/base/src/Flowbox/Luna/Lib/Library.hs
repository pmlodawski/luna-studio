---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Flowbox.Luna.Lib.Library where

import           Flowbox.Luna.Data.AST.Module  (Module)
import qualified Flowbox.Luna.Data.AST.Module  as Module
import qualified Flowbox.Luna.Data.AST.Type    as Type
import           Flowbox.Luna.Data.PropertyMap (PropertyMap)
import qualified Flowbox.Luna.Data.PropertyMap as PropertyMap
import           Flowbox.Prelude
import           Flowbox.System.UniPath        (UniPath)



data Library = Library { _name        :: String
                       , _path        :: UniPath
                       , _ast         :: Module
                       , _propertyMap :: PropertyMap
                       } deriving (Show, Read)

makeLenses (''Library)

type ID  = Int


make :: String -> UniPath -> [String] -> Library
make name' path' modulePath = Library name' path' emptyModule PropertyMap.empty where
    emptyModule = Module.mk 0 $ Type.mkModule 1 modulePath

