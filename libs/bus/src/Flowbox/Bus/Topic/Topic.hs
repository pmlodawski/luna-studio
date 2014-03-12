---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Bus.Topic.Topic where

import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Char             (toLower, toUpper)
import           Data.String.Utils     (replace)
import qualified Text.Read             as Read

import Flowbox.Prelude



type Topic = ByteString


fromTopic :: (Show a, Read a) => Topic -> Either String a
fromTopic = Read.readEither . unwords . map capitalize . words . replaceDots . unpack
  where capitalize (x:xs) = toUpper x : map toLower xs
        capitalize []     = []
        replaceDots = replace "." " "


toTopic :: (Show a, Read a) => a -> Topic
toTopic = pack . map toLower . replaceSpace . show
  where replaceSpace = replace " " "."


mk :: String -> Topic
mk = pack

str :: Topic -> String
str = unpack
