---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AWS.User.User where

import Flowbox.AWS.User.Password (Password)
import Flowbox.Prelude



type Name = String


data User = User { name     :: Name
                 , password :: Password
                 } deriving (Show, Read)


fromDB :: (Name, Password) -> User
fromDB (name', password') = User name' password'


toDB :: User -> (Name, Password)
toDB (User name' password') = (name', password')
