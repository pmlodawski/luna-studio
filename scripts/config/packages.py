###########################################################################
## Copyright (C) Flowbox, Inc / All Rights Reserved
## Unauthorized copying of this file, via any medium is strictly prohibited
## Proprietary and confidential
## Flowbox Team <contact@flowbox.io>, 2014
###########################################################################

import os
from subprocess import call, Popen, PIPE
from utils.colors import print_error
from utils.errors import fatal

rootPath = os.path.dirname(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))

def handle_error(e):
    if e:
        print_error(e)
        fatal()

class Project(object):
    def __init__(self, name='', path='', binpath='', deps=None):
        if deps == None: deps = []
        self.name    = name
        self.path    = path
        self.binpath = binpath
        self.sbox    = os.path.join(rootPath, 'dist', self.path)
        self.deps    = set(deps)

    def install(self):   pass

    def uninstall(self): pass

    def targets(self):
        return [self]

    def target_binpaths(self):
        paths = []
        for target in self.targets():
            paths.append(target.binpath)
        return paths

    def target_names(self):
        names = []
        for target in self.targets():
            names.append(target.name)
        return names

class HProject(Project):
    def install(self):
        cmd = 'cabal sandbox add-source ../../../%s' % self.path
        (out, err) = Popen(cmd, stdout=PIPE, stderr=PIPE, shell=True).communicate()
        handle_error(err)
        return out

    def uninstall(self):
        cmd = 'cabal sandbox hc-pkg unregister %s' % self.name
        (out, err) = Popen(cmd, stdout=PIPE, stderr=PIPE, shell=True).communicate()
        if err:
            err = err.replace('.exe', '')
            if not err.startswith('ghc-pkg: cannot find package'):
                handle_error(err)
        return out


class AllProject(Project):
    def targets(self):
        return pkgDb.values()

pkgDb = { '@all'               : AllProject ('@all', deps = [])
       , 'libs/cabal-install'  : HProject   ('cabal-install'       , os.path.join ('libs'  , 'cabal-install') , 'libs'    , []                                                  )
       , 'libs/target-hs'      : HProject   ('luna-target-hs'      , os.path.join ('libs'  , 'target-hs')     , 'libs'    , []                                                  )
       , 'libs/utils'          : HProject   ('flowbox-utils'       , os.path.join ('libs'  , 'utils')         , 'libs'    , []                                                  )
       , 'libs/config'         : HProject   ('flowbox-config'      , os.path.join ('libs'  , 'config')        , 'libs'    , ['libs/utils']                                           )
       , 'tools/broker'        : HProject   ('flowbox-broker'      , os.path.join ('tools' , 'broker')        , 'tools'   , ['libs/utils']                                           )
       , 'tools/wrappers'      : HProject   ('flowbox-wrappers'    , os.path.join ('tools' , 'wrappers')      , 'wrappers', ['libs/config']                                          )
       , 'libs/luna'           : HProject   ('flowbox-luna'        , os.path.join ('libs'  , 'luna')          , 'libs'    , ['libs/target-hs', 'libs/utils', 'libs/config', 'libs/cabal-install', 'libs/markup']      )
       , 'tools/initializer'   : HProject   ('flowbox-initializer' , os.path.join ('tools' , 'initializer')   , 'tools'   , ['libs/utils', 'libs/config']                                 )
       , 'tools/lunac'         : HProject   ('flowbox-lunac'       , os.path.join ('tools' , 'lunac')         , 'tools'   , ['libs/utils', 'libs/config', 'tools/initializer', 'libs/luna']          )
       , 'libs/batch'          : HProject   ('flowbox-batch'       , os.path.join ('libs'  , 'batch')         , 'libs'    , ['libs/utils', 'libs/config', 'tools/initializer', 'libs/luna']          )
       , 'tools/batch-srv'     : HProject   ('flowbox-batch-srv'   , os.path.join ('tools' , 'batch-srv')     , 'tools'   , ['libs/utils', 'libs/config', 'tools/initializer', 'libs/luna', 'libs/batch'] )
       , 'libs/num-conversion' : HProject   ('num-conversion'      , os.path.join ('libs'  , 'num-conversion'), 'libs'    , []                                                  )
       , 'libs/graphics'       : HProject   ('flowbox-graphics'    , os.path.join ('libs'  , 'graphics')      , 'libs'    , ['libs/target-hs', 'libs/utils', 'libs/num-conversion']            )
       , 'libs/markup'         : HProject   ('doc-markup'          , os.path.join ('libs'  , 'markup')        , 'libs'    , []                                                  )
       }
