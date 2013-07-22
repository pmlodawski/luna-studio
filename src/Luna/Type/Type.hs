---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Type.Type(
Type(..)
) where


data Type = Undefined
          | TypeVariable {name   :: String}
          | Class        {name   :: String}
          | Function     {name   :: String, inputs :: [Type], outputs :: [Type]}
          | Tuple        {items  :: [Type]}
          | List         {item   :: Type  }
          | Interface    {fields :: [Type], methods :: [Type]}
          | Package      {name   :: String}
          deriving (Show)