---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AWS.User where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.Digest.Pure.SHA as SHA
import           Data.Map             (Map)
import qualified Data.Map             as Map

import Flowbox.Prelude



type UserName = String
type Password = SHA.Digest SHA.SHA256State
type BatchAddress = String
type Error = String

data UserData = UserData { password :: Password
                         } deriving (Show, Eq, Ord)


type UserDatabase = Map UserName UserData


register :: UserName -> ByteString -> UserDatabase -> Either Error UserDatabase
register userName password' database = case Map.lookup userName database of
    Just _  -> Left "Cannot register user: username already exists"
    Nothing -> do let userData = UserData $ SHA.sha256 password'
                  Right $ Map.insert userName userData database



login :: UserName -> ByteString -> UserDatabase -> IO (Either Error BatchAddress)
login userName password' database = case Map.lookup userName database of
    Nothing              -> return $ Left "Login failed: no such user"
    Just (UserData hash) -> if hash /= SHA.sha256 password'
                                then return $ Left "Login failed: password incorrect"
                                else fail "login : Not implemented"
