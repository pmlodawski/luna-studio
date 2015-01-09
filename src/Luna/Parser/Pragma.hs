---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable        #-}


module Luna.Parser.Pragma where


import           Data.Typeable

import           Flowbox.Prelude

import qualified Luna.Parser.Token       as Tok



pragma = Tok.pragma