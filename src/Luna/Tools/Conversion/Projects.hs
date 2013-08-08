---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Luna.Tools.Conversion.Projects where

import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict   (HashMap)
import           Data.Text.Lazy        (Text, pack, unpack)

import qualified Attrs_Types             as TAttrs
import           Luna.Network.Flags        (Flags(..))
import qualified Luna.Network.Attributes as Attributes
import           Luna.Network.Attributes   (Attributes)
import           Luna.Tools.Conversion
