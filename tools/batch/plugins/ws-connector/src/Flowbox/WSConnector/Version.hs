module Flowbox.WSConnector.Version where

import           Flowbox.Prelude
import qualified Data.Version               as Version
import qualified Flowbox.WSConnector.Config as Config

full :: Bool -> String
full numeric = prefix ++ Version.showVersion Config.version where
    prefix = if numeric
             then ""
             else "Flowbox websocket connector version "

