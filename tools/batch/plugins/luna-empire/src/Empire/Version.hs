module Empire.Version where

import qualified Data.Version as Version

import qualified Empire.Config as Config
import           Flowbox.Prelude



full :: Bool -> String
full = logger


logger :: Bool -> String
logger numeric = (if numeric then "" else "Flowbox bus logger version ") ++ Version.showVersion Config.version
