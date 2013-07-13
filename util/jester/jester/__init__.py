from collections import defaultdict
import argparse
from jester import logger as jlogger
import sys
import inspect
import subprocess
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

    # def target(self, name):



import types

class PyConfig(object):
    def __init__(self, path):
        self.path = path
        self.reset()

    def reset(self):
        self.__funcs = defaultdict(lambda: lambda(ctx): None)

    def load(self):
        with open(self.path, 'r') as file:
            s = file.read()
        exec s
        self.reset()
        for k,v in locals().iteritems():
            if isinstance(v, types.FunctionType):
                self.__funcs[k] = v

    def func(self, name):
        return self.__funcs[name]

class Context(object):
    toolchain = None
    config    = None
    target    = None

class Jester(object):
    def __init__(self):
        self.__toolchains = defaultdict(lambda: None)
        self.__toolchain_actions = []
        self.__target_actions = []

        self.__parser = argparse.ArgumentParser(description='Compile multitarget Haskell code.')


    def init(self):
        self.__parser.add_argument('--verbose', '-v', action='store_true', help='Verbose mode.')
        self.__parser.add_argument('--target', '-t', nargs=1, default=[None], help='Destination target.')
        args = self.__parser.parse_known_args()[0]
        jlogger.init(args.verbose)
        self.parsers     = self.__parser.add_subparsers()

    def run(self):
        cfg = PyConfig('util/jester/jester/config/default.py')
        cfg.load()
        ctx = Context()
        ctx.config = cfg

        cfg.func('init')(ctx)

        if not ctx.toolchain:
            logger.error("No toolchain defined.")
            return
        logger.debug("Searching for toolchain '%s' definition" % ctx.toolchain)
        toolchain = jester.toolchain(ctx.toolchain)
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

        # call toolchain function
        args.func(ctx)
        # print args.func
        # print args


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
            def action():
                logger.debug("Registering toolchain '%s'" % name)
                self.__toolchains[name] = ToolChain(name, cls)
            self.__toolchain_actions.append(action)
        return dec

    def register_target(self, toolchain, name):
        def register_dec(cls):
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