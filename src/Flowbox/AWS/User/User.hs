---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.AWS.User.User where

import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import Database.PostgreSQL.Simple.ToField (ToField, toField)
import Database.PostgreSQL.Simple.ToRow   (ToRow, toRow)

import Flowbox.AWS.User.Password (Password (Password))
import Flowbox.Prelude



type Name = String


data User = User { _name     :: Name
                 , _password :: Password
                 , _credit   :: Int     -- 1000 = 1$
                 } deriving (Show)


makeLenses (''User)


instance FromRow User where
    fromRow = User <$> field <*> (Password  <$> field <*> field) <*> field

instance ToRow User where
    toRow (User name' password' credit') =
        toField name' : toRow password' ++ [toField credit']
