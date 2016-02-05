---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Luna.Syntax.Enum where

import           Flowbox.Prelude

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

type ID = Int

newtype IDTag = IDTag ID deriving (Show, Generic, Num, Eq, Read)

class Enumerated a where
    id  :: a -> ID
    tag :: ID -> a

instance Enumerated IDTag where
    id (IDTag i) = i
    tag          = IDTag

instance Enumerated Int where
    id  i = i
    tag i = i
