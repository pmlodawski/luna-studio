import argparse
import cPickle
import subprocess
import json
import jester.cabal.configparser as cabalparser
import os
import fnmatch
from collections import defaultdict
from jester import jester
import sys

import logging
logger = logging.getLogger(__name__)

jester.init()

@jester.register_target('cabal', 'js')
class HasteCompiler:
    compiler = 'haste-inst'

    def configure(self, ctx):
        jester.call(ctx.compiler, 'configure', *ctx.args)

    def build(self, ctx):
        jester.call(ctx.compiler, 'build', *ctx.args)

    def clean(self, ctx):
        jester.call(ctx.compiler, 'clean', *ctx.args)


@jester.register_target('cabal', 'native')
class CabalTarget:
    compiler = 'cabal'

    def configure(self, ctx):
        jester.call(ctx.compiler, 'configure', *ctx.args)

    def build(self, ctx):
        jester.call(ctx.compiler, 'build', *ctx.args)

    def clean(self, ctx):
        jester.call(ctx.compiler, 'clean', *ctx.args)




@jester.register_toolchain('cabal')
class CabalToolchain:
    def configure(self, ctx):
        ctx.target.configure(ctx)

    def build(self, ctx):
        ctx.target.build(ctx)

    def clean(self, ctx):
        ctx.target.clean(ctx)

jester.resolve()







def main():
    try:
        jester.run()
    except Exception as e:
        logger.error(str(e))


    pass

main()


# import sys
# sys.exit()
#
# class ConfigObject(object):
#     def __init__(self, path):
#         self.path = path
#
#     def serialize(self):
#         return json.dumps(self.__dict__)
#
#     def deserialize(self, s):
#         self.__dict__.update(json.loads(s))
#         return self
#
#     def store(self):
#         with open (self.path, 'w') as file:
#             file.write(self.serialize())
#
#     def load(self):
#         with open (self.path, 'r') as file:
#             self.deserialize(file.read())
#
#
# class UserConfig(ConfigObject):
#     default_target = 'native'
#     compilers      = {'native':'cabal', 'js':'haste-inst'}
#
#
# class ProjectConfig(ConfigObject):
#     target    = None
#     main      = None
#     compiler  = None
#
#
# def glob_recursive(pathlist, regex='*.*'):
#     paths = [os.path.join(dirpath, f)
#     for path in pathlist
#     for dirpath, dirnames, files in os.walk(path)
#     for f in fnmatch.filter(files, regex)]
#     return list(set([os.path.normpath(path) for path in paths]))
#
#
# def call(args):
#     logger.debug("Calling: '%s'."%(' '.join(args)))
#     return subprocess.call(args)
#
# def configure(args, subargs):
#     # fix the argparser
#     args.target = args.target[0]
#
#     logger.debug('Searching for cabal configuration file')
#     cabalconf_path = find_file('.cabal')
#     if not cabalconf_path:
#         logger.error("[Error] No '.cabal' config file found.")
#         return
#     logger.debug("Found Cabal configuration file '%s'." % cabalconf_path)
#
#     logger.debug("Reading Cabal configuration file.")
#     with open (cabalconf_path, 'r') as file:
#         cabalconf = file.read()
#
#     logger.debug("Parsing Cabal configuration file.")
#     config = cabalparser.parse(cabalconf)
#
#     main_name     = config['Main-Is'][0]
#     src_paths     = config['Hs-source-dirs']
#
#     logger.debug("Searching for main '%s' files in src paths: %s" % (main_name, src_paths))
#     mainpaths = glob_recursive(src_paths, main_name)
#
#     mainpaths_len = len(mainpaths)
#     if mainpaths_len == 1:
#         mainpath = mainpaths[0]
#         logger.debug("Found main file: %s." % repr(mainpath))
#     else:
#         logger.error("Cannot identify main source file. Found %s candidates: %s" %(mainpaths_len, mainpaths))
#         return
#
#     userconf = UserConfig('jester.config')
#     try:    userconf.load()
#     except: logger.warning("No 'jester.config' file found. Using default configuration.")
#     projconf           = ProjectConfig('.jester')
#     projconf.main      = mainpath
#     projconf.target    = args.target if args.target else userconf.default_target
#     projconf.compiler  = userconf.compilers[projconf.target]
#     logger.debug("Confiuration target '%s'." % projconf.target)
#     projconf.store()
#
#     ###############################
#
#     logger.debug("Using compiler '%s'."%projconf.compiler)
#
#     call([projconf.compiler, 'configure']+subargs)
#
#
# def build(args, subargs):
#     run_compiler(['build']+subargs)
#
# def clean(args, subargs):
#     run_compiler(['clean']+subargs)
#     os.remove('.jester')
#
#
# def run_compiler(args):
#     projconf = ProjectConfig('.jester')
#     try:    projconf.load()
#     except:
#         logger.error("Run the 'configure' command first")
#         return
#     logger.debug("Using compiler '%s'."%projconf.compiler)
#     status = call([projconf.compiler]+args)
#     if status != 0:
#         raise Exception ("Command error")
#
#
# parser           = argparse.ArgumentParser(description='Compile multitarget Haskell code.')
# action_parsers   = parser.add_subparsers()
#
# parser_configure = action_parsers.add_parser('configure', help='Configure project.')
# parser_configure.set_defaults(func=configure)
# parser_configure.add_argument('--target', '-t', nargs=1, choices=['native', 'js'], default=[None],
#                             help='Target compilation platform')
# parser_build     = action_parsers.add_parser('build',     help='Build project.')
# parser_build.set_defaults(func=build)
# parser_clean     = action_parsers.add_parser('clean',     help='Clean project.')
# parser_clean.set_defaults(func=clean)
#
#
#
# def read_jester_config():
#     with open ('.jester', 'r') as file:
#         cfgtxt = file.read()
#
# def find_file(suffix):
#     import os
#     cwd =  os.getcwd()
#     files = os.listdir(cwd)
#
#     configfile = None
#     for file in files:
#         if file.endswith(suffix):
#             configfile = file
#             break
#     return configfile
#
#
#
# def main():
#     args, subargs= parser.parse_known_args()
#     try:
#         args.func(args, subargs)
#     except Exception as e:
#         print "Error occured: %s."%e
#
#     # try:
#     #     read_jester_config()
#     # except:
#     #     print "Run the 'configure' command first"
#     #     return
#
# main()



# def s_ident(scanner, token): return token
# def s_operator(scanner, token): return "op%s" % token
# def s_float(scanner, token): return float(token)
# def s_int(scanner, token): return int(token)



# scanner = re.Scanner([
#     (r"[a-zA-Z_]\w*", s_ident),
#     (r"\d+\.\d*", s_float),
#     (r"\d+", s_int),
#     (r"=|\+|-|\*|/", s_operator),
#     (r"\s+", None),
#     ])

# print scanner.scan("sum = 3*foo + 312.50 + bar")