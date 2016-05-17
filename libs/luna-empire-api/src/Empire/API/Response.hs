module Empire.API.Response where

import           Prologue

import           Data.Binary (Binary)

data Status a = Ok    { _result  :: a }
              | Error { _message :: String }
              deriving (Generic, Show, Eq)

instance (Binary a) => Binary (Status a)
makeLenses ''Status
makePrisms ''Status

data Response req res = Response { _request  :: req
                                 , _status   :: Status res
                                 }
                      deriving (Generic, Show, Eq)

type SimpleResponse req = Response req ()

resultOk :: req -> res -> Response req res
resultOk req payload = Response req (Ok payload)
ok :: req -> Response req ()
ok       req         = Response req (Ok ())
error :: req -> String -> Response req ()
error    req msg     = Response req (Error msg)

makeLenses ''Response

instance (Binary req, Binary res) => Binary (Response req res)
