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


data User = User { _id       :: Int
                 , _name     :: Name
                 , _password :: Password
                 , _credit   :: Int
                 } deriving (Show)


makeLenses (''User)


fromDB :: (Int, String, String, String, Int) -> User
fromDB (id', name', salt', hash', credit') =
    User id' name' (Password salt' hash') credit'


toDB :: User -> (String, String, String, Int)
toDB (User _ name' password' credit') =
    (name', password' ^. Password.salt, password' ^. Password.hash, credit')
