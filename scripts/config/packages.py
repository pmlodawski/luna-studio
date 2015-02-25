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
from utils.system import system, systems
import sys
import config.sandbox as sbox


rootPath = os.path.dirname(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))

def handle_error(e):
    if e:
        print_error(e)
        fatal()


class Flag(object):
    def __init__(self, content, systems=None):
        self.content   = content
        self.systems = systems

class Flags(object):
    def __init__(self, flags=None):
        if flags     == None: flags = []
        self.flags   = flags

    def get(self):
        fs = []
        for flag in self.flags:
            if flag.systems == None or system in flag.systems:
                fs.append(flag.content)
        return fs


class Project(object):
    def __init__(self, sbox, name='', path='', binpath='', deps=None, flags=None):
        self.sbox    = sbox(rootPath, path)
        if deps  == None: deps = []
        if flags == None: flags = Flags()
        self.name    = name
        self.path    = path
        self.binpath = binpath
        self.deps    = set(deps)
        self.flags   = flags

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
        cmd = 'cabal sandbox add-source %s' % os.path.join(rootPath, self.path)
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
        # It is needed to omit non-project entries with no path (like @all)
        return [project for project in pkgDb.values() if project.path]

class CoreLunaPlatform(Project):
    def targets(self):
        return [project for project in corePkgDb.values() if project.path]

corePkgDb = \
       { 'libs/batch/batch'                    : HProject   (sbox.glob, 'flowbox-batch'                , os.path.join ('libs' , 'batch', 'batch')                      , 'libs'    , ['libs/config', 'libs/luna/core', 'libs/luna/distribution-old', 'libs/luna/initializer', 'libs/luna/pass-old', 'libs/luna/protobuf-old', 'libs/utils'])
       , 'libs/luna/core'                      : HProject   (sbox.glob, 'luna-core'                    , os.path.join ('libs' , 'luna', 'core')                        , 'libs'    , ['libs/utils'])
       , 'libs/utils'                          : HProject   (sbox.glob, 'flowbox-utils'                , os.path.join ('libs' , 'utils')                               , 'libs'    , [])
       # , 'libs/luna/typechecker'               : HProject   ('luna-typechecker'             , os.path.join ('libs' , 'luna', 'typechecker')                 , 'libs'    , ['libs/logger', 'libs/luna/core', 'libs/luna/parser3', 'libs/luna/pass2'])
       }

pkgDb = dict(corePkgDb, **{
         '@all'                                : AllProject (sbox.glob, '@all', deps = [])
       , '@core'                               : CoreLunaPlatform (sbox.glob, '@core', deps = [])
       , 'libs/aws'                            : HProject   (sbox.glob, 'flowbox-aws'                  , os.path.join ('libs' , 'aws')                                 , 'libs'    , ['libs/rpc', 'libs/utils'])
       , 'libs/batch/plugins/project-manager'  : HProject   (sbox.glob, 'batch-lib-project-manager'    , os.path.join ('libs' , 'batch', 'plugins', 'project-manager') , 'libs'    , ['libs/batch/batch', 'libs/bus', 'libs/config', 'libs/luna/core', 'libs/rpc', 'libs/utils'])
       , 'libs/batch/plugins/file-manager'     : HProject   (sbox.glob, 'batch-lib-file-manager'       , os.path.join ('libs' , 'batch', 'plugins', 'file-manager')    , 'libs'    , ['libs/bus', 'libs/config', 'libs/rpc', 'libs/utils'])
       , 'libs/bus'                            : HProject   (sbox.glob, 'flowbox-bus'                  , os.path.join ('libs' , 'bus')                                 , 'libs'    , ['libs/config', 'libs/rpc', 'libs/utils'])
       , 'libs/config'                         : HProject   (sbox.glob, 'flowbox-config'               , os.path.join ('libs' , 'config')                              , 'libs'    , ['libs/utils'])
       , 'libs/data/codec/exr'                 : HProject   (sbox.glob, 'openexr'                      , os.path.join ('libs' , 'data', 'codec', 'exr')                , 'libs'    , [], flags=Flags([Flag('--with-gcc=g++')]))
       , 'libs/data/dynamics/particles'        : HProject   (sbox.glob, 'particle'                     , os.path.join ('libs' , 'data', 'dynamics', 'particles')       , 'libs'    , [])
       , 'libs/data/graphics'                  : HProject   (sbox.glob, 'flowbox-graphics'             , os.path.join ('libs' , 'data', 'graphics')                    , 'libs'    , ['third-party/accelerate', 'third-party/accelerate-cuda', 'third-party/accelerate-io', 'third-party/accelerate-fft', 'third-party/algebraic', 'third-party/imagemagick', 'libs/utils', 'libs/num-conversion', 'libs/data/serialization', 'third-party/linear-accelerate', 'libs/luna/target/ghchs', 'libs/data/codec/exr'], flags=Flags([Flag("--with-gcc=g++", [systems.LINUX]),Flag("--with-gcc=gcc-4.9", [systems.DARWIN]), Flag("-fcuda")])) # FIXME [kl]: The fcuda flag is a temporary solution for the strange cabal behavior
       , 'libs/data/accelerate/thrust'         : HProject   (sbox.glob, 'accelerate-thrust'            , os.path.join ('libs' , 'data', 'accelerate', 'thrust')        , 'libs'    , ['third-party/accelerate', 'third-party/accelerate-cuda'])
       , 'libs/data/serialization'             : HProject   (sbox.glob, 'flowbox-serialization'        , os.path.join ('libs' , 'data', 'serialization')               , 'libs'    , ['libs/luna/target/ghchs', 'libs/utils'])
       , 'libs/doc/markup'                     : HProject   (sbox.glob, 'doc-markup'                   , os.path.join ('libs' , 'doc', 'markup')                       , 'libs'    , [])
       , 'libs/luna/core'                      : HProject   (sbox.glob, 'luna-core'                    , os.path.join ('libs' , 'luna', 'core')                        , 'libs'    , ['libs/utils'])
       , 'libs/luna/build'                     : HProject   (sbox.glob, 'luna-build'                   , os.path.join ('libs' , 'luna', 'build')                       , 'libs'    , ['libs/config', 'libs/luna/core', 'libs/luna/distribution', 'libs/luna/pass', 'libs/utils'])
       , 'libs/luna/distribution'              : HProject   (sbox.glob, 'luna-distribution'            , os.path.join ('libs' , 'luna', 'distribution')                , 'libs'    , ['libs/config', 'libs/luna/core', 'libs/utils'])
       , 'libs/luna/distribution-old'          : HProject   (sbox.glob, 'luna-distribution-old'        , os.path.join ('libs' , 'luna', 'distribution-old')            , 'libs'    , ['libs/config', 'libs/luna/core', 'libs/utils'])
       , 'libs/luna/interpreter'               : HProject   (sbox.glob, 'luna-interpreter'             , os.path.join ('libs' , 'luna', 'interpreter')                 , 'libs'    , ['libs/batch/batch', 'libs/data/serialization', 'libs/luna/core', 'libs/luna/distribution-old', 'libs/luna/pass', 'libs/luna/pass-old', 'libs/utils'])
       , 'libs/luna/interpreter-old'           : HProject   (sbox.glob, 'luna-interpreter-old'         , os.path.join ('libs' , 'luna', 'interpreter-old')             , 'libs'    , ['libs/config', 'libs/luna/core', 'libs/luna/pass', 'libs/utils'])
       , 'libs/luna/initializer'               : HProject   (sbox.glob, 'luna-initializer'             , os.path.join ('libs' , 'luna', 'initializer')                 , 'libs'    , ['libs/config', 'libs/utils'])
       , 'libs/luna/pass'                      : HProject   (sbox.glob, 'luna-pass'                    , os.path.join ('libs' , 'luna', 'pass')                        , 'libs'    , ['libs/config', 'libs/luna/core', 'libs/luna/target/ghchs', 'libs/utils'])
       , 'libs/luna/pass-old'                  : HProject   (sbox.glob, 'luna-pass-old'                , os.path.join ('libs' , 'luna', 'pass-old')                    , 'libs'    , ['libs/config', 'libs/luna/core', 'libs/luna/distribution-old', 'libs/luna/parser2-old', 'libs/luna/target/ghchs', 'libs/utils'])
       , 'libs/luna/parser2-old'               : HProject   (sbox.glob, 'luna-parser2-old'             , os.path.join ('libs' , 'luna', 'parser2-old')                 , 'libs'    , ['libs/luna/core', 'libs/utils'])
       , 'libs/luna/protobuf'                  : HProject   (sbox.glob, 'luna-protobuf'                , os.path.join ('libs' , 'luna', 'protobuf')                    , 'libs'    , ['libs/luna/core', 'libs/utils', 'libs/config', 'libs/luna/distribution'])
       , 'libs/luna/protobuf-old'              : HProject   (sbox.glob, 'luna-protobuf-old'            , os.path.join ('libs' , 'luna', 'protobuf-old')                , 'libs'    , ['libs/luna/core', 'libs/utils', 'libs/config', 'libs/luna/distribution-old'])
       , 'libs/num-conversion'                 : HProject   (sbox.glob, 'num-conversion'               , os.path.join ('libs' , 'num-conversion')                      , 'libs'    , [])
       , 'libs/repo-manager'                   : HProject   (sbox.glob, 'flowbox-repo-manager'         , os.path.join ('libs' , 'repo-manager')                        , 'libs'    , ['libs/bus', 'libs/config', 'libs/rpc', 'libs/utils'])
       , 'libs/rpc'                            : HProject   (sbox.glob, 'flowbox-rpc'                  , os.path.join ('libs' , 'rpc')                                 , 'libs'    , ['libs/utils'])
       , 'libs/task-queue'                     : HProject   (sbox.glob, 'task-queue'                   , os.path.join ('libs' , 'task-queue')                          , 'libs'    , ['libs/utils'])
       , 'libs/luna/target/ghchs'              : HProject   (sbox.glob, 'luna-target-ghchs'            , os.path.join ('libs' , 'luna', 'target', 'ghchs')             , 'libs'    , ['libs/utils'])
       , 'tools/aws/account-manager'           : HProject   (sbox.glob, 'flowbox-account-manager'      , os.path.join ('tools', 'aws', 'account-manager')              , 'tools'   , ['libs/aws', 'libs/rpc', 'libs/utils'])
       , 'tools/aws/account-manager-mock'      : HProject   (sbox.glob, 'flowbox-account-manager-mock' , os.path.join ('tools', 'aws', 'account-manager-mock')         , 'tools'   , ['libs/aws', 'libs/rpc', 'libs/utils'])
       , 'tools/aws/instance-manager'          : HProject   (sbox.glob, 'flowbox-instance-manager'     , os.path.join ('tools', 'aws', 'instance-manager')             , 'tools'   , ['libs/aws', 'libs/utils' ])
       , 'tools/batch/plugins/broker'          : HProject   (sbox.glob, 'batch-plugin-broker'          , os.path.join ('tools', 'batch', 'plugins', 'broker')          , 'tools'   , ['libs/bus', 'libs/config', 'libs/rpc', 'libs/utils'])
       , 'tools/batch/plugins/bus-logger'      : HProject   (sbox.glob, 'batch-plugin-bus-logger'      , os.path.join ('tools', 'batch', 'plugins', 'bus-logger')      , 'tools'   , ['libs/bus', 'libs/config', 'libs/rpc', 'libs/utils'])
       , 'tools/batch/plugins/interpreter'     : HProject   (sbox.glob, 'batch-plugin-interpreter'     , os.path.join ('tools', 'batch', 'plugins', 'interpreter')     , 'tools'   , ['libs/batch/batch', 'libs/batch/plugins/project-manager', 'libs/bus', 'libs/config', 'libs/luna/interpreter', 'libs/rpc', 'libs/utils'])
       , 'tools/batch/plugins/file-manager'    : HProject   (sbox.glob, 'batch-plugin-file-manager'    , os.path.join ('tools', 'batch', 'plugins', 'file-manager')    , 'tools'   , ['libs/batch/plugins/file-manager', 'libs/bus', 'libs/config', 'libs/rpc', 'libs/utils'])
       , 'tools/batch/plugins/parser'          : HProject   (sbox.glob, 'batch-plugin-parser'          , os.path.join ('tools', 'batch', 'plugins', 'parser')          , 'tools'   , ['libs/batch/batch', 'libs/bus', 'libs/config', 'libs/luna/core', 'libs/rpc', 'libs/utils'])
       , 'tools/batch/plugins/plugin-manager'  : HProject   (sbox.glob, 'batch-plugin-plugin-manager'  , os.path.join ('tools', 'batch', 'plugins', 'plugin-manager')  , 'tools'   , ['libs/bus', 'libs/config', 'libs/rpc', 'libs/utils'])
       , 'tools/batch/plugins/project-manager' : HProject   (sbox.glob, 'batch-plugin-project-manager' , os.path.join ('tools', 'batch', 'plugins', 'project-manager') , 'tools'   , ['libs/batch/batch', 'libs/batch/plugins/project-manager', 'libs/bus', 'libs/config', 'libs/luna/core', 'libs/rpc', 'libs/utils'])
       , 'tools/batch/plugins/s3-file-manager' : HProject   (sbox.glob, 'batch-plugin-s3-file-manager' , os.path.join ('tools', 'batch', 'plugins', 's3-file-manager') , 'tools'   , ['libs/aws', 'libs/batch/batch', 'libs/batch/plugins/file-manager', 'libs/bus', 'libs/config', 'libs/luna/core', 'libs/rpc', 'libs/utils'])
       , 'tools/initializer'                   : HProject   (sbox.glob, 'flowbox-initializer-cli'      , os.path.join ('tools', 'initializer')                         , 'tools'   , ['libs/config', 'libs/luna/initializer', 'libs/utils'])
       , 'tools/lunac'                         : HProject   (sbox.glob, 'luna-compiler'                , os.path.join ('tools', 'lunac')                               , 'tools'   , ['libs/config', 'libs/luna/build', 'libs/luna/core', 'libs/luna/distribution', 'libs/luna/initializer', 'libs/luna/pass', 'libs/utils'])
       , 'tools/wrappers'                      : HProject   (sbox.glob, 'flowbox-wrappers'             , os.path.join ('tools', 'wrappers')                            , 'wrappers', ['libs/config'])
       , 'third-party/algebraic'               : HProject   (sbox.glob, 'algebraic'                    , os.path.join ('third-party', 'algebraic')                     , 'third-party', ['third-party/accelerate'])
       , 'third-party/accelerate'              : HProject   (sbox.glob, 'accelerate'                   , os.path.join ('third-party', 'accelerate')                    , 'third-party', [])
       , 'third-party/accelerate-cuda'         : HProject   (sbox.glob, 'accelerate-cuda'              , os.path.join ('third-party', 'accelerate-cuda')               , 'third-party', ['third-party/mainland-pretty'], flags=Flags([Flag('-fdebug')])) # [KL] accelerate debug flag is necessary to dump generated CUDA kernels
       , 'third-party/accelerate-fft'          : HProject   (sbox.glob, 'accelerate-fft'               , os.path.join ('third-party', 'accelerate-fft')                , 'third-party', [])
       , 'third-party/accelerate-io'           : HProject   (sbox.glob, 'accelerate-io'                , os.path.join ('third-party', 'accelerate-io')                 , 'third-party', [])
       , 'third-party/imagemagick'             : HProject   (sbox.glob, 'imagemagick'                  , os.path.join ('third-party', 'imagemagick')                   , 'third-party', []) # [KL] temporary fix until imagemagick is fixed
       , 'third-party/linear-accelerate'       : HProject   (sbox.glob, 'linear-accelerate'            , os.path.join ('third-party', 'linear-accelerate')             , 'third-party', ['third-party/accelerate']) # [MM] not so temporary fix, included because of too strict upper bound on accelerate
       , 'third-party/mainland-pretty'         : HProject   (sbox.glob, 'mainland-pretty'              , os.path.join ('third-party', 'mainland-pretty')               , 'third-party', []) # [MM] temporary fix until mainland-pretty relaxes upper bound on text to allow version 1.2
       })
