###########################################################################
## Copyright (C) Flowbox, Inc # All Rights Reserved
## Unauthorized copying of this file, via any medium is strictly prohibited
## Proprietary and confidential
## Flowbox Team <contact@flowbox.io>, 2013
###########################################################################

import types

import logging
logger = logging.getLogger(__name__)

class Config(object):
    def __getattr__(self, name):
        logger.warning("Accessing non existing configuration member '%s'." % name)
        return lambda(ctx): None

class PyConfigManager(object):
    def __init__(self, path):
        self.path = path
        self.__code = '#empty'

    def load_file(self):
        with open(self.path, 'r') as f:
            self.__code = f.read()

    def load(self):
        exec self.__code
        d = {}
        for k,v in locals().iteritems():
            if isinstance(v, types.FunctionType):
                d[k] = v
        conf = Config()
        conf.__dict__.update(d)
        return conf

    def store(self):
        with open(self.path, 'w') as f:
            f.write(self.__code)

    def set(self, code):
        self.__code = code
