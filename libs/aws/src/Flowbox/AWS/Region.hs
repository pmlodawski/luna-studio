---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Flowbox.AWS.Region where

import           Data.Text (Text)
import qualified Data.Text as Text

import Flowbox.Prelude



data Region = AsiaPacific_Tokyo
            | AsiaPacific_Singapore
            | AsiaPacific_Sydney
            | EU_Ireland
            | SouthAmerica_SaoPaulo
            | USEast_NorthernVirginia
            | USWest_NorthernCalifornia
            | USWest_Oregon
            | Other Text


toText :: Region -> Text
toText region = case region of
    AsiaPacific_Tokyo           -> "ap-northeast-1"
    AsiaPacific_Singapore       -> "ap-southeast-1"
    AsiaPacific_Sydney          -> "ap-southeast-2"
    EU_Ireland                  -> "eu-west-1"
    SouthAmerica_SaoPaulo       -> "sa-east-1"
    USEast_NorthernVirginia     -> "us-east-1"
    USWest_NorthernCalifornia   -> "us-west-1"
    USWest_Oregon               -> "us-west-2"
    Other name                  -> name


fromText :: Text -> Region
fromText text = case text of
    "ap-northeast-1" -> AsiaPacific_Tokyo
    "ap-southeast-1" -> AsiaPacific_Singapore
    "ap-southeast-2" -> AsiaPacific_Sydney
    "eu-west-1"      -> EU_Ireland
    "sa-east-1"      -> SouthAmerica_SaoPaulo
    "us-east-1"      -> USEast_NorthernVirginia
    "us-west-1"      -> USWest_NorthernCalifornia
    "us-west-2"      -> USWest_Oregon
    name             -> Other name


fromString :: String -> Region
fromString = fromText . Text.pack
