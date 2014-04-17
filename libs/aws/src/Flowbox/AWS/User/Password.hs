---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AWS.User.Password where

import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Digest.Pure.SHA       as SHA

import Flowbox.Prelude



type Password = SHA.Digest SHA.SHA256State


type Plain = String


mk :: Plain -> Password
mk = SHA.sha256 . pack
