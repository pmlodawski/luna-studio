---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Projects where

import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict   (HashMap)
import           Data.Text.Lazy        (Text, pack, unpack)

import qualified Attrs_Types             as TAttrs
import           Flowbox.Luna.Network.Flags        (Flags(..))
import qualified Flowbox.Luna.Network.Attributes as Attributes
import           Flowbox.Luna.Network.Attributes   (Attributes)
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion


instance Convert Project TProjects.Project where
    encode project = tproject where
    	