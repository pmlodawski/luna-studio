###########################################################################
## Copyright (C) Flowbox, Inc # All Rights Reserved
## Unauthorized copying of this file, via any medium is strictly prohibited
## Proprietary and confidential
## Flowbox Team <contact@flowbox.io>, 2013
###########################################################################

import logging
logger = logging.getLogger(__name__)

def init(ctx):
    ctx.toolchain = 'cabal'
    ctx.target    = 'native'

def configure(ctx):
    pass

def build(ctx):
    pass

def clean(ctx):
    pass

def postbuild(ctx):
    import os
    import shutil
    if ctx.target.name == 'js':
        main_path, _ = os.path.splitext(ctx.main)
        main = main_path + '.js'
        if not os.path.exists(main):
            logger.error("Result js file '%s' does not exist." % main)
        binname   = ctx.executable + '.js'
        buildpath = os.path.join('dist', 'build', binname)
        maindst = os.path.join(buildpath,binname)
        try:
            os.makedirs(buildpath)
        except:
            pass
        logger.debug("Moving result file to '%s'" % maindst)
        shutil.move(main, maindst)
    pass