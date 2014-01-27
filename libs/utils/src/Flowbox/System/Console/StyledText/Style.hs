---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.System.Console.StyledText.Style (
        module Flowbox.System.Console.StyledText.Style,
        module System.Console.ANSI
) where

import Flowbox.Prelude

import           System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR)
import qualified System.Console.ANSI as ANSI


data Style = Style { layer     :: ConsoleLayer
                   , intensity :: ColorIntensity
                   , color     :: Color
                   }
            | Reset
            deriving (Show)


toSGR :: Style -> [SGR]
toSGR s = case s of
        Style l i c -> pure $ ANSI.SetColor l i c
        Reset       -> pure $ ANSI.Reset
