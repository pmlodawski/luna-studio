{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Empire.API.Response where

import           Prologue

import           Data.Binary (Binary)
import           Empire.API.Topic (MessageTopic)

data Status a = Ok    { _resultData  :: a }
              | Error { _message     :: String }
              deriving (Generic, Show, Eq)

instance (Binary a) => Binary (Status a)
makeLenses ''Status
makePrisms ''Status

data Response req res = Response { _request  :: req
                                 , _status   :: Status res
                                 }
                      deriving (Generic, Show, Eq)

type SimpleResponse req = Response req ()

class (MessageTopic req, MessageTopic (Response req res), Binary req, Binary res) => ResponseResult req res | req -> res where
  result :: req -> res -> Response req res
  result req payload = Response req (Ok payload)

  error :: req -> String -> Response req res
  error    req msg     = Response req (Error msg)

ok :: (ResponseResult req (), MessageTopic (Response req ())) => req -> Response req ()
ok req = Response req (Ok ())

makeLenses ''Response

instance (Binary req, Binary res) => Binary (Response req res)
