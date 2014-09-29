
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}

module Flowbox.Text.ProtocolBuffers (
    module Proto,
    Serializable,
    getExt',
    messagePut',
    messageGet',
    mkExtField,
) where

import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Lazy            as ByteString
import qualified Data.Map                        as Map
import           Text.ProtocolBuffers            as Proto
import qualified Text.ProtocolBuffers.Extensions as Extensions

import Flowbox.Prelude



type Serializable msg = (Proto.ReflectDescriptor msg, Proto.Wire msg)


getExt' :: Extensions.Key Maybe msg val -> msg -> Either String val
getExt' key msg = case Extensions.getExt key msg of
    Right (Just args) -> return args
    Right Nothing     -> fail   "Error while getting extension"
    Left   e          -> fail $ "Error while getting extension: " ++ e


messagePut' :: Serializable msg => msg -> ByteString
messagePut' = ByteString.toStrict . Proto.messagePut


messageGet' :: Serializable msg => ByteString -> Either String msg
messageGet' msg = fmap fst $ Proto.messageGet $ ByteString.fromStrict msg


mkExtField :: Extensions.ExtField
mkExtField = Extensions.ExtField Map.empty
