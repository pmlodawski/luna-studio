module Empire.Version where

import qualified Data.Version    as Version

import qualified Empire.Config   as Config
import           Flowbox.Prelude

fullVersion :: String
fullVersion = "Flowbox bus logger version " ++ Version.showVersion Config.version
