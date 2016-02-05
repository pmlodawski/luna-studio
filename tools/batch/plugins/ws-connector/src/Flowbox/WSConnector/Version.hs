module Flowbox.WSConnector.Version where

import qualified Data.Version               as Version
import           Flowbox.Prelude
import qualified Flowbox.WSConnector.Config as Config

full :: Bool -> String
full numeric = prefix ++ Version.showVersion Config.version where
    prefix = if numeric
             then ""
             else "Flowbox websocket connector version "

