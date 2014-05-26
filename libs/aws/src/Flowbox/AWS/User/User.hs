---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.AWS.User.User where

import           Flowbox.AWS.User.Password (Password (Password))
import qualified Flowbox.AWS.User.Password as Password
import           Flowbox.Prelude



type Name = String


data User = User { name     :: Name
                 , password :: Password
                 } deriving (Show)


makeLenses (''User)


fromDB :: (Name, String) -> User
fromDB (name', password') = User name' (Password password')


toDB :: User -> (Name, String)
toDB (User name' password') = (name', Password.fromPassword password')
