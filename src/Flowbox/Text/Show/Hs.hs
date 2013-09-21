---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Text.Show.Hs (
	hsShow
) where

import           Flowbox.Prelude   


hsShow :: String -> String
hsShow = _hsShow 0

indent :: Int
indent = 4

newline :: Int -> String
newline i = "\n" ++ replicate (indent*i) ' '

_hsShow :: Int -> String -> String
_hsShow _ []     = []
_hsShow i (x:xs) = case x of
	'{' -> x : newline (i+1) ++     _hsShow (i+1) xs
	';' -> x : newline (i)   ++     _hsShow i xs
	',' -> x : newline (i)   ++     _hsShow i xs
	'}' ->     newline (i-1) ++ x : _hsShow (i-1) xs
	_   -> x : _hsShow i xs

