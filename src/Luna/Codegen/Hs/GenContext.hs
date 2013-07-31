---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.GenContext (
    GenContext,
    empty,
    modIndend,
    incIndend,
    decIndend
)where


data GenContext = GenContext { indent :: Int }
          deriving (Show)

empty :: GenContext
empty = GenContext 0


modIndend :: Int -> GenContext -> GenContext
modIndend i ctx = GenContext $ indent ctx + i


incIndend :: GenContext -> GenContext
incIndend = modIndend 1


decIndend :: GenContext -> GenContext
decIndend = modIndend (-1)