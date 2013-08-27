---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import           Prelude                   hiding (error)
import           Flowbox.System.Log.Logger   

logger = getLogger "MyApp.BuggyComponent"

main = do
        logger.setLevel  $ DEBUG
        logger.debug     $ "debug"
        logger.info      $ "info"
        logger.notice    $ "notice"
        logger.warning   $ "warning"
        logger.error     $ "error"
        logger.critical  $ "critical"
        logger.alert     $ "alert"
        logger.emergency $ "emergency"
