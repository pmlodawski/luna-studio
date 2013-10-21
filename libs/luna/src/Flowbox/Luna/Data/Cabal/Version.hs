---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Cabal.Version where

import           Numeric
import           Flowbox.Prelude   


data Version = Version { major :: Int
                       , minor :: Int 
                       , patch :: Int
                       }

instance Read Version where
    readsPrec _ s = [(Version ma mi pa, r) | (ma,  r1) <- readDec s,
                                             (".", r2) <- lex     r1,
                                             (mi,  r3) <- readDec r2,
                                             (".", r4) <- lex     r3,
                                             (pa,  r)  <- readDec r4]

instance Show Version where
    showsPrec _ (Version ma mi pa) = shows ma . ('.':) . shows mi . ('.':) . shows pa

