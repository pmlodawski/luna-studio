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



newtype Password = Password {fromPassword :: String}
                   deriving (Eq)


instance Show Password where
    show = show . fromPassword


type Plain = String


mk :: Plain -> Password
mk = Password . SHA.showDigest . SHA.sha256 . pack


verify :: Password -> Plain -> Bool
verify password plain = password == mk plain

