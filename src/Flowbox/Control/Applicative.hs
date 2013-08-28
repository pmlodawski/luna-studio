---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Control.Applicative (
	module Control.Applicative,
	many1,
	many2
) where

import           Control.Applicative   

many1 :: Alternative f => f a -> f [a]
many1 p = (:) <$> p <*> many p

many2 :: Alternative f => f a -> f [a]
many2 p = (:) <$> p <*> many1 p