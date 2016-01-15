module Empire.API.Update where

import           Prologue

import           Data.Binary (Binary)

data ResultOk = Ok deriving (Generic, Show, Eq)

instance Binary ResultOk

data Update req res = Update { _request  :: req
                             , _result   :: res
                             }
                      deriving (Generic, Show, Eq)

type SimpleUpdate req = Update req ResultOk

makeLenses ''Update

instance (Binary req, Binary res) => Binary (Update req res)
