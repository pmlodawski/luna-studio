module ZMQ.Bus.Control.BusCtx where

import           Control.Applicative
import           Data.IORef          (IORef)
import qualified Data.IORef          as IORef

import           Flowbox.Prelude



newtype BusCtx = BusCtx { nextSenderID :: IORef Int }


empty :: IO BusCtx
empty = BusCtx <$> IORef.newIORef 0
