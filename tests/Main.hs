---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import           Prelude                   hiding (error)
import           Flowbox.System.Log.Logger   
import qualified Flowbox.System.Log.Logger as Logger
import qualified Flowbox.Data.Version      as Version


logger :: Logger
logger = getLogger "MyApp.BuggyComponent"

main :: IO ()
main = do
        logger.setLevel  $ DEBUG
        logger.debug     $ "debug"
        Logger.pushLogGroup logger
        logger.info      $ "info"
        Logger.popLogGroup logger
        logger.notice    $ "notice"
        logger.warning   $ "warning"
        logger.error     $ "error"
        logger.critical  $ "critical"
        logger.alert     $ "alert"
        logger.emergency $ "emergency"

        Logger.enableColorOutput False logger
        logger.debug     $ "debug"
        Logger.pushLogGroup logger
        logger.info      $ "info"
        Logger.popLogGroup logger
        logger.notice    $ "notice"
        logger.warning   $ "warning"
        logger.error     $ "error"
        logger.critical  $ "critical"
        logger.alert     $ "alert"
        logger.emergency $ "emergency"

        let
        	v1 = Version.Version 0 1 0 Version.Alpha
        	v2 = Version.mk { Version.minor = 1
                            , Version.stage = Version.Alpha
                            }
        print v1
        print $ Version.str v1

