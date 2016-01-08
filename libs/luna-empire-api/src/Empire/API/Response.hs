module Empire.API.Response where

import           Prologue

import           Data.Binary (Binary)

data ResultOk = Ok deriving (Generic, Show, Eq)

instance Binary ResultOk

data Response req upd = Update    { _request  :: req
                                  , _update   :: upd
                                  }
                      | Exception { _request  :: req
                                  , _message  :: String
                                  }
                      deriving (Generic, Show, Eq)

type SimpleResponse req = Response req ResultOk

makeLenses ''Response
makePrisms ''Response

-- _UpdatePayload :: Getter (Response req upd) upd -- FIXME: Jak to otypowac?
-- _UpdatePayload = _Update . _2

-- _UpdateRequest :: Getter (Response req upd) req
-- _UpdateRequest = _Update . _1

instance (Binary req, Binary upd) => Binary (Response req upd)
