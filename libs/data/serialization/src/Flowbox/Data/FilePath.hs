---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Data.FilePath where

import Data.String.Utils (split)
import Text.Printf       (printf)
import Text.Read         (readMaybe)

import Flowbox.Prelude



applyTime :: Float -> FilePath -> FilePath
applyTime time name = case split "$" name of
    [] -> ""
    h:t -> h ++ concatMap replace t
    where
        replace :: FilePath -> FilePath
        replace ('F':'F':n:rest) = formatFloat n ++ rest
        replace ('F':n:rest)     = formatInt   n ++ rest
        replace other            = other

        formatFloat :: Char -> FilePath
        formatFloat n = case readMaybe [n] :: Maybe Int of
            Just _  -> printf ("%." ++ [n] ++ "f") time
            Nothing -> printf  "%f" time

        formatInt :: Char -> FilePath
        formatInt n = case readMaybe [n] :: Maybe Int of
            Just _  -> printf ("%." ++ [n] ++ "d") intTime
            Nothing -> printf  "%d" intTime

        intTime :: Int
        intTime = round time

