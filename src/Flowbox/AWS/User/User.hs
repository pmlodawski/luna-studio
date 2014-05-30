---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.AWS.User.User where

import Flowbox.AWS.User.Password (Password)
import Flowbox.Prelude



type Name = String


data User = User { _name     :: Name
                 , _password :: Password
                 , _credit   :: Int
                 } deriving (Show)


makeLenses (''User)

