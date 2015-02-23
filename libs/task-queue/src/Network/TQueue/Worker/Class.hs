{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Worker.Class where

import Control.Lens
import Data.Binary (Binary)
import GHC.Generics (Generic)

data Worker = Worker { _responeAddress :: Maybe String
                     } deriving (Show, Generic)

makeLenses ''Worker

instance Binary Worker