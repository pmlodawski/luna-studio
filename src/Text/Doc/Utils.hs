---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Doc.Utils where

import Data.Monoid
import Text.Parsec hiding (many, optional, parse, (<|>))



(++) :: Monoid a => a -> a -> a
(++) = mappend

surround :: Stream s m t => ParsecT s u m close -> ParsecT s u m a -> ParsecT s u m a
surround a = between a a
