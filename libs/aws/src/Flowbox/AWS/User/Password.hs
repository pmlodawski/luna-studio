---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.AWS.User.Password where

import           Data.ByteString.Lazy.Char8         (pack)
import qualified Data.Digest.Pure.SHA               as SHA
import           Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import           Database.PostgreSQL.Simple.ToField (toField)
import           Database.PostgreSQL.Simple.ToRow   (ToRow, toRow)

import Flowbox.Prelude



type Salt  = String
type Plain = String

data Password = Password { _salt :: Salt
                         , _hash :: String
                         } deriving (Eq, Show)


makeLenses (''Password)


mk :: Salt -> Plain -> Password
mk salt' plain = Password salt' $ SHA.showDigest $ SHA.sha256 $ pack $ plain ++ salt'


verify :: Password -> Plain -> Bool
verify password plain = password == mk plain (password ^. salt)



instance FromRow Password where
    fromRow = Password <$> field <*> field

instance ToRow Password where
    toRow (Password salt' hash') =
        [toField salt', toField hash']
