###########################################################################
## Copyright (C) Flowbox, Inc # All Rights Reserved
## Unauthorized copying of this file, via any medium is strictly prohibited
## Proprietary and confidential
## Flowbox Team <contact@flowbox.io>, 2013
###########################################################################

from collections import defaultdict
import argparse
from jester import logger as jlogger
import sys
import inspect
import subprocess

from jester.config import default
from core.config import PyConfigManager

import logging
logger = logging.getLogger(__name__)

class Jester(object):
    def __init__(self):
        self.__toolchains = defaultdict(lambda: None)
        self.__toolchain_actions = []
        self.__target_actions = []
        self.__parser = argparse.ArgumentParser(description='Compile multitarget Haskell code.')
        self.cfg_manager = PyConfigManager('jesterconf.py')
        self.cfg_manager.set(inspect.getsource(default))

    def init(self):
        self.__parser.add_argument('--verbose', '-v', action='store_true', help='Verbose mode.')
        self.__parser.add_argument('--target', '-t', nargs=1, default=[None], help='Destination target.')
        self.__parser.add_argument('--generate-config', action='store_true', help='Generate default jester config.')
        args = self.__parser.parse_known_args()[0]
        jlogger.init(args.verbose)
        self.parsers     = self.__parser.add_subparsers()
        self.parsers.add_parser('jester').set_defaults(func=lambda(ctx):None)
        if args.generate_config:
            self.cfg_manager.store()

    def run(self):
        try:
            self.cfg_manager.load_file()
        except:
            logger.warning("No project configuration found. Using default one. Use 'jester --generate-config' to generate default configuration.")
        cfg = self.cfg_manager.load()
        ctx = Context()
        ctx.config = cfg
        cfg.init(ctx)

        if not ctx.toolchain:
            logger.error("No toolchain defined.")
            return
        logger.debug("Searching for toolchain '%s' definition" % ctx.toolchain)
        toolchain = self.toolchain(ctx.toolchain)
        if not toolchain:
            logger.error("Toolchain '%s' definition not found." % ctx.toolchain)
            return
        logger.debug("Found toolchain '%s' definition in '%s'" %(ctx.toolchain, sys.modules[toolchain.__module__].__file__))
        toolchain.init()

        args, rest = self.__parser.parse_known_args()
        args.target = args.target[0] # fix argparse
        ctx.args = rest

        if args.target:
            ctx.target = args.target
        if not ctx.target:
            raise Exception("No target specified")

        logger.debug("Using target '%s'" % ctx.target)
        target_defs = toolchain.target(ctx.target)

        logger.debug("Found %s target definition(s)" % (len(target_defs)))
        logger.debug("Checking for needed tools")
        for target_def in target_defs:
            binpath = target_def.check()
            if binpath:
                ctx.compiler = binpath
                ctx.target   = target_def.get_instance()
                break

        if not binpath:
            raise Exception("No tools found")

        logger.debug("Executing toolchain commands.")
        args.func(ctx)


    def toolchain(self, name):
        return self.__toolchains[name]

    def call(self, *args):
        args = list(args)
        logger.debug("Calling: '%s'." % (' '.join(args)))
        rcode = subprocess.call(args)
        if rcode != 0:
            raise Exception("Command failed.")

    def register_toolchain(self, name):
        def dec(cls):
            cls.name = name
            def action():
                logger.debug("Registering toolchain '%s'" % name)
                self.__toolchains[name] = ToolChain(name, cls)
            self.__toolchain_actions.append(action)
        return dec

    def register_target(self, toolchain, name):
        def register_dec(cls):
            cls.name = name
            def action():
                logger.debug("Registering target '%s'" % name)
                self.__toolchains[toolchain].add_target(name, Target(name,cls))
            self.__target_actions.append(action)
        return register_dec

    def resolve(self):
        logger.debug('Registering toolchains')
        for action in self.__toolchain_actions:
            action()

        logger.debug('Registering targets')
        for action in self.__target_actions:
            action()

jester = Jester()

from core.context import Context
from core.toolchain import ToolChain
from core.target import Target





