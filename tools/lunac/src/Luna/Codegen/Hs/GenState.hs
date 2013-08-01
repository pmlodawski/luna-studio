---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.GenState (
    GenState,
    empty,
    modIndend,
    incIndend,
    decIndend
)where


data GenState = GenState { indent :: Int }
          deriving (Show)

empty :: GenState
empty = GenState 0


modIndend :: Int -> GenState -> GenState
modIndend i ctx = GenState $ indent ctx + i


incIndend :: GenState -> GenState
incIndend = modIndend 1


decIndend :: GenState -> GenState
decIndend = modIndend (-1)