---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE OverlappingInstances #-}

module Data.Convertible.Instances.Missing where

import           Prelude
import           Data.Convertible
import           Data.ByteString
import qualified Data.ByteString.Char8  as Char8
import qualified Data.Text.Lazy         as LText
import qualified Data.Text.Lazy.Builder as LText
import qualified Data.Text              as Text


instance Convertible LText.Text LText.Builder where
    safeConvert = Right . LText.fromLazyText

instance Convertible String LText.Builder where
    safeConvert = Right . LText.fromLazyText . convert

instance Convertible LText.Builder LText.Text where
    safeConvert = Right . LText.toLazyText

instance Convertible LText.Builder String where
    safeConvert = Right . convert . LText.toLazyText


instance Convertible Text.Text LText.Builder where
    safeConvert = Right . LText.fromText

instance Convertible LText.Builder Text.Text where
    safeConvert = Right . convert . LText.toLazyText


instance Convertible a b => Convertible [a] [b] where
    safeConvert = sequence . fmap safeConvert