---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Luna.AST.Comment where

import           Control.Lens
import           Flowbox.Generics.Deriving.QShow
import           Flowbox.Prelude
import           GHC.Generics (Generic)

newtype Comment = Comment { _txt :: String }
         deriving (Show, Eq, Generic, Read)

instance QShow Comment
makeLenses (''Comment)





