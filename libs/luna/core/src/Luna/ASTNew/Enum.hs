---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Luna.ASTNew.Enum where

import Flowbox.Prelude
import GHC.Generics    (Generic)


----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

type ID = Int

data IDTag = IDTag ID deriving (Show, Eq, Generic)

class Enumerated a where
    id :: a -> ID

instance Enumerated IDTag where
    id (IDTag i) = i
    