module Empire.API.Response where

import Prologue

import Data.Binary (Binary)

data ResultOk = Ok deriving (Generic, Show, Eq)

instance Binary ResultOk

data Response req res = Result    { _request  :: req
                                  , _result   :: res
                                  }
                      | Exception { _request  :: req
                                  , _message  :: String
                                  }
                      deriving (Generic, Show, Eq)

type SimpleResponse req = Response req ResultOk

makeLenses ''Response

instance (Binary req, Binary res) => Binary (Response req res)
