###########################################################################
## Copyright (C) Flowbox, Inc # All Rights Reserved
## Unauthorized copying of this file, via any medium is strictly prohibited
## Proprietary and confidential
## Flowbox Team <contact@flowbox.io>, 2013
###########################################################################

from jester import jester
from collections import defaultdict
import inspect

import logging
logger = logging.getLogger(__name__)

class ToolChain(object):
    def __init__(self, name, cls):
        self.__name    = name
        self.__cls     = cls
        self.__targets = defaultdict(list)

    def init(self):
        logger.debug("Initialising toolchain '%s'" % self.__name)
        self.__tool = self.__cls()
        for name, func in inspect.getmembers(self.__tool, predicate=inspect.ismethod):
            jester.parsers.add_parser(name).set_defaults(func=func)
        logger.debug("Toolchain '%s' initialised." % self.__name)

    def add_target(self, name, cls):
        self.__targets[name].append(cls)

    def targets(self):
        return self.__targets.keys()

    def target(self, name):
        return self.__targets[name]