---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AWS.User.User where

import qualified Data.Digest.Pure.SHA as SHA

import Flowbox.Prelude



type Name = String


type Password = SHA.Digest SHA.SHA256State


data Data = Data { password :: Password
                 } deriving (Show, Eq, Ord)

