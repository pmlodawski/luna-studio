from collections import defaultdict
import argparse
from jester import logger

class Jester(object):
    def __init__(self):
        """

        :rtype : object
        """
        self.targets = defaultdict(list)


    def init(self):
        parser           = argparse.ArgumentParser(description='Compile multitarget Haskell code.')
        parser.add_argument('--verbose', '-v', action='store_true', help='Verbose mode.')
        args = parser.parse_known_args()[0]
        logger.init(args.verbose)

        self.parsers     = parser.add_subparsers()


    def register_target(self, name):
        def register_dec(cls):
            self.targets[name].append(cls)
        return register_dec

jester = Jester()