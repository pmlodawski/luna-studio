###########################################################################
## Copyright (C) Flowbox, Inc # All Rights Reserved
## Unauthorized copying of this file, via any medium is strictly prohibited
## Proprietary and confidential
## Flowbox Team <contact@flowbox.io>, 2013
###########################################################################

from jester import jester
from jester.cabal import cabalparser
from glob import glob
from jester.utils.path import glob_recursive

import logging
logger = logging.getLogger(__name__)

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


#TODO[wd]: This function should probably be run only by configuration and store its results somewhere
def collect_cabal_info(ctx):
    logger.debug('Searching for cabal configuration file.')
    cabalconf_paths = glob('*.cabal')
    cabalconf_num = len(cabalconf_paths)
    if cabalconf_num == 0:
        raise Exception("No cabal configuration file found")
    elif cabalconf_num >1:
        raise Exception("Found more than one cabal configuration file: %s I do not know what to do :(" % cabalconf_paths)
    cabalconf_path = cabalconf_paths[0]

    logger.debug("Reading Cabal configuration file.")
    with open (cabalconf_path, 'r') as f:
        cabalconf = f.read()

    logger.debug("Parsing Cabal configuration file.")
    config = cabalparser.parse(cabalconf)

    main_name      = config['main-is'][0]
    src_paths      = config['hs-source-dirs']
    ctx.executable = config['executable'][0]

    logger.debug("Searching for main '%s' files in src paths: %s" % (main_name, src_paths))
    mainpaths = glob_recursive(src_paths, main_name)

    mainpaths_len = len(mainpaths)
    if mainpaths_len == 1:
        mainpath = mainpaths[0]
        logger.debug("Found main file: %s." % repr(mainpath))
    else:
        raise Exception("Cannot identify main source file. Found %s candidates: %s" %(mainpaths_len, mainpaths))
    ctx.main = mainpath


@jester.register_toolchain('cabal')
class CabalToolchain:
    def configure(self, ctx):
        collect_cabal_info(ctx)
        ctx.config.configure(ctx)
        ctx.target.configure(ctx)

    def build(self, ctx):
        collect_cabal_info(ctx)
        ctx.config.build(ctx)
        ctx.target.build(ctx)
        ctx.config.postbuild(ctx)

    def clean(self, ctx):
        collect_cabal_info(ctx)
        ctx.config.clean(ctx)
        ctx.target.clean(ctx)