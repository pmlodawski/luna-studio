###########################################################################
## Copyright (C) Flowbox, Inc # All Rights Reserved
## Unauthorized copying of this file, via any medium is strictly prohibited
## Proprietary and confidential
## Flowbox Team <contact@flowbox.io>, 2013
###########################################################################

from jester.utils.sys import which

import logging
logger = logging.getLogger(__name__)

class Target(object):
    def __init__(self, name, cls):
        self.__name = name
        self.__cls  = cls

    def check(self):
        logger.info("Checking for %s" % self.__cls.compiler)
        binpath = which(self.__cls.compiler)
        if not binpath:
            logger.info("Not found.")
        else:
            logger.info("Found: %s" % binpath)
        return binpath

    def get_instance(self):
        return self.__cls()