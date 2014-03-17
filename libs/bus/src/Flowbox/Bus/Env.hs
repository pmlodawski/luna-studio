---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Bus.Env where

import qualified System.ZMQ4.Monadic as ZMQ

import qualified Flowbox.Bus.Message as Message



data BusEnv z = BusEnv { subSocket  :: ZMQ.Socket z ZMQ.Sub
                       , pushSocket :: ZMQ.Socket z ZMQ.Push
                       , clientID   :: Message.ClientID
                       , requestID  :: Message.RequestID
                       }


