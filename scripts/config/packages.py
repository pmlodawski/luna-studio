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
    def __init__(self, name='', path='', binpath='', deps=None, flags=None):
        if deps  == None: deps = []
        if flags == None: flags = Flags()
        self.name    = name
        self.path    = path
        self.binpath = binpath
        self.sbox    = os.path.join(rootPath, 'dist', self.path)
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
       { 'libs/batch/batch'                    : HProject   ('flowbox-batch'                , os.path.join ('libs' , 'batch', 'batch')                      , 'libs'    , ['libs/config', 'libs/luna/core', 'libs/luna/distribution-old', 'libs/luna/initializer', 'libs/luna/pass-old', 'libs/luna/protobuf-old', 'libs/utils', "libs/convert"])
       , 'libs/luna/core'                      : HProject   ('luna-core'                    , os.path.join ('libs' , 'luna', 'core')                        , 'libs'    , ['libs/utils', "libs/convert"])
       , 'libs/utils'                          : HProject   ('flowbox-utils'                , os.path.join ('libs' , 'utils')                               , 'libs'    , ['third-party/fgl', "libs/convert"])
       , 'libs/luna/typechecker'               : HProject   ('luna-tc'             , os.path.join ('libs' , 'luna', 'typechecker')                 , 'libs'    , ['libs/utils', 'libs/luna/core', 'libs/luna/pass', "libs/convert", 'third-party/graphviz-2999.17.0.2.2'])
       }

pkgDb = dict(corePkgDb, **{
         '@all'                                : AllProject ('@all', deps = [])
       , '@core'                               : CoreLunaPlatform ('@core', deps = [])
       , 'libs/aws'                            : HProject   ('flowbox-aws'                  , os.path.join ('libs' , 'aws')                                 , 'libs'    , ['libs/rpc', 'libs/utils', "libs/convert"])
       , 'libs/convert'                        : HProject   ('convert'                      , os.path.join ('libs' , 'convert')                             , 'libs'    , [])
       , 'libs/batch/plugins/project-manager'  : HProject   ('batch-lib-project-manager'    , os.path.join ('libs' , 'batch', 'plugins', 'project-manager') , 'libs'    , ['libs/batch/batch', 'libs/batch/plugins/project-manager-api', 'libs/bus', 'libs/config', 'libs/luna/core', 'libs/rpc', 'libs/utils', 'libs/batch/plugins/ur-manager', "libs/convert"])
       , 'libs/batch/plugins/project-manager-api': HProject('batch-lib-project-manager-api', os.path.join ('libs' , 'batch', 'plugins', 'project-manager-api') , 'libs' , ['libs/batch/batch', 'libs/luna/protobuf-old', "libs/convert"])
       , 'libs/batch/plugins/file-manager'     : HProject   ('batch-lib-file-manager'       , os.path.join ('libs' , 'batch', 'plugins', 'file-manager')    , 'libs'    , ['libs/bus', 'libs/config', 'libs/rpc', 'libs/utils', "libs/convert"])
       , 'libs/batch/plugins/ur-manager'       : HProject   ('batch-lib-ur-manager'         , os.path.join ('libs' , 'batch', 'plugins', 'ur-manager')      , 'libs'    , ['libs/bus', "libs/convert"])
       , 'libs/bus'                            : HProject   ('flowbox-bus'                  , os.path.join ('libs' , 'bus')                                 , 'libs'    , ['libs/config', 'libs/rpc', 'libs/utils', "libs/convert"])
       , 'libs/config'                         : HProject   ('flowbox-config'               , os.path.join ('libs' , 'config')                              , 'libs'    , ['libs/utils', "libs/convert"])
       , 'libs/data/codec/exr'                 : HProject   ('openexr'                      , os.path.join ('libs' , 'data', 'codec', 'exr')                , 'libs'    , [], flags=Flags([Flag("--with-gcc=g++", [systems.LINUX, "libs/convert"]),Flag("--with-gcc=gcc-4.9", [systems.DARWIN, "libs/convert"]), "libs/convert"]))
       , 'libs/data/dynamics/particles'        : HProject   ('particle'                     , os.path.join ('libs' , 'data', 'dynamics', 'particles')       , 'libs'    , ["libs/convert"])
       , 'libs/data/graphics'                  : HProject   ('flowbox-graphics'             , os.path.join ('libs' , 'data', 'graphics')                    , 'libs'    , ['libs/data/codec/exr', 'libs/data/serialization', 'libs/luna/target/ghchs', 'libs/num-conversion', 'libs/utils', 'third-party/accelerate', 'third-party/accelerate-cuda', 'third-party/accelerate-fft', 'third-party/accelerate-io', 'third-party/algebraic', 'third-party/binary', 'third-party/imagemagick', 'third-party/linear-accelerate'], flags=Flags([Flag("--with-gcc=g++", [systems.LINUX, "libs/convert"]),Flag("--with-gcc=gcc-4.9", [systems.DARWIN, "libs/convert"]), Flag("-fcuda"), "libs/convert"])) # FIXME [kl]: The fcuda flag is a temporary solution for the strange cabal behavior
       , 'libs/data/accelerate/thrust'         : HProject   ('accelerate-thrust'            , os.path.join ('libs' , 'data', 'accelerate', 'thrust')        , 'libs'    , ['third-party/accelerate', 'third-party/accelerate-cuda', "libs/convert"])
       , 'libs/data/serialization'             : HProject   ('flowbox-serialization'        , os.path.join ('libs' , 'data', 'serialization')               , 'libs'    , ['libs/luna/target/ghchs', 'libs/utils', "libs/convert"])
       , 'libs/doc/markup'                     : HProject   ('doc-markup'                   , os.path.join ('libs' , 'doc', 'markup')                       , 'libs'    , ["libs/convert"])
       , 'libs/gui-mockup'                     : HProject   ('flowbox-gui-mockup'           , os.path.join ('libs' , 'gui-mockup')                          , 'libs'    , ['third-party/algebraic', 'third-party/binary', "libs/convert"])
       , 'libs/luna/core'                      : HProject   ('luna-core'                    , os.path.join ('libs' , 'luna', 'core')                        , 'libs'    , ['libs/utils', "libs/convert"])
       , 'libs/luna/build'                     : HProject   ('luna-build'                   , os.path.join ('libs' , 'luna', 'build')                       , 'libs'    , ['libs/config', 'libs/luna/core', 'libs/luna/distribution', 'libs/luna/pass', 'libs/utils', "libs/convert"])
       , 'libs/luna/distribution'              : HProject   ('luna-distribution'            , os.path.join ('libs' , 'luna', 'distribution')                , 'libs'    , ['libs/config', 'libs/luna/core', 'libs/utils', "libs/convert"])
       , 'libs/luna/distribution-old'          : HProject   ('luna-distribution-old'        , os.path.join ('libs' , 'luna', 'distribution-old')            , 'libs'    , ['libs/config', 'libs/luna/core', 'libs/utils', "libs/convert"])
       , 'libs/luna/interpreter'               : HProject   ('luna-interpreter'             , os.path.join ('libs' , 'luna', 'interpreter')                 , 'libs'    , ['libs/batch/batch', 'libs/data/serialization', 'libs/luna/core', 'libs/luna/distribution-old', 'libs/luna/pass', 'libs/luna/pass-old', 'libs/utils', 'third-party/HMap', "libs/convert"])
       , 'libs/luna/interpreter-old'           : HProject   ('luna-interpreter-old'         , os.path.join ('libs' , 'luna', 'interpreter-old')             , 'libs'    , ['libs/config', 'libs/luna/core', 'libs/luna/pass', 'libs/utils', "libs/convert"])
       , 'libs/luna/interpreter-runtime'       : HProject   ('luna-interpreter-runtime'     , os.path.join ('libs' , 'luna', 'interpreter-runtime')         , 'libs'    , ['libs/data/serialization', 'libs/luna/target/ghchs', 'libs/utils', 'third-party/HMap', "libs/convert"])
       , 'libs/luna/initializer'               : HProject   ('luna-initializer'             , os.path.join ('libs' , 'luna', 'initializer')                 , 'libs'    , ['libs/config', 'libs/utils', "libs/convert"])
       , 'libs/luna/pass'                      : HProject   ('luna-pass'                    , os.path.join ('libs' , 'luna', 'pass')                        , 'libs'    , ['libs/config', 'libs/luna/core', 'libs/luna/target/ghchs', 'libs/utils', "libs/convert"])
       , 'libs/luna/pass-old'                  : HProject   ('luna-pass-old'                , os.path.join ('libs' , 'luna', 'pass-old')                    , 'libs'    , ['libs/config', 'libs/luna/core', 'libs/luna/distribution-old', 'libs/luna/parser2-old', 'libs/luna/target/ghchs', 'libs/utils', "libs/convert"])
       , 'libs/luna/parser2-old'               : HProject   ('luna-parser2-old'             , os.path.join ('libs' , 'luna', 'parser2-old')                 , 'libs'    , ['libs/luna/core', 'libs/utils', "libs/convert"])
       , 'libs/luna/protobuf'                  : HProject   ('luna-protobuf'                , os.path.join ('libs' , 'luna', 'protobuf')                    , 'libs'    , ['libs/luna/core', 'libs/utils', 'libs/config', 'libs/luna/distribution', "libs/convert"])
       , 'libs/luna/protobuf-old'              : HProject   ('luna-protobuf-old'            , os.path.join ('libs' , 'luna', 'protobuf-old')                , 'libs'    , ['libs/luna/core', 'libs/utils', 'libs/config', 'libs/luna/distribution-old', "libs/convert"])
       , 'libs/num-conversion'                 : HProject   ('num-conversion'               , os.path.join ('libs' , 'num-conversion')                      , 'libs'    , ["libs/convert"])
       , 'libs/repo-manager'                   : HProject   ('flowbox-repo-manager'         , os.path.join ('libs' , 'repo-manager')                        , 'libs'    , ['libs/bus', 'libs/config', 'libs/rpc', 'libs/utils', "libs/convert"])
       , 'libs/rpc'                            : HProject   ('flowbox-rpc'                  , os.path.join ('libs' , 'rpc')                                 , 'libs'    , ['libs/utils', "libs/convert"])
       , 'libs/task-queue'                     : HProject   ('task-queue'                   , os.path.join ('libs' , 'task-queue')                          , 'libs'    , ['libs/utils', "libs/convert"])
       , 'libs/luna/target/ghchs'              : HProject   ('luna-target-ghchs'            , os.path.join ('libs' , 'luna', 'target', 'ghchs')             , 'libs'    , ['libs/utils', "libs/convert"])
       , 'libs/ws-connector'                   : HProject   ('flowbox-ws-connector'         , os.path.join ('libs' , 'ws-connector')                        , 'libs'    , ['libs/bus', 'libs/config', 'libs/rpc', 'libs/utils', "libs/convert"])

       , 'tools/aws/account-manager'           : HProject   ('flowbox-account-manager'      , os.path.join ('tools', 'aws', 'account-manager')              , 'tools'   , ['libs/aws', 'libs/rpc', 'libs/utils', "libs/convert"])
       , 'tools/aws/account-manager-mock'      : HProject   ('flowbox-account-manager-mock' , os.path.join ('tools', 'aws', 'account-manager-mock')         , 'tools'   , ['libs/aws', 'libs/rpc', 'libs/utils', "libs/convert"])
       , 'tools/aws/instance-manager'          : HProject   ('flowbox-instance-manager'     , os.path.join ('tools', 'aws', 'instance-manager')             , 'tools'   , ['libs/aws', 'libs/utils' , "libs/convert"])
       , 'tools/batch/plugins/broker'          : HProject   ('batch-plugin-broker'          , os.path.join ('tools', 'batch', 'plugins', 'broker')          , 'tools'   , ['libs/bus', 'libs/config', 'libs/rpc', 'libs/utils', "libs/convert"])
       , 'tools/batch/plugins/bus-logger'      : HProject   ('batch-plugin-bus-logger'      , os.path.join ('tools', 'batch', 'plugins', 'bus-logger')      , 'tools'   , ['libs/bus', 'libs/config', 'libs/rpc', 'libs/utils', "libs/convert"])
       , 'tools/batch/plugins/interpreter'     : HProject   ('batch-plugin-interpreter'     , os.path.join ('tools', 'batch', 'plugins', 'interpreter')     , 'tools'   , ['libs/batch/batch', 'libs/batch/plugins/project-manager', 'libs/bus', 'libs/config', 'libs/luna/interpreter', 'libs/rpc', 'libs/utils', "libs/convert"])
       , 'tools/batch/plugins/file-manager'    : HProject   ('batch-plugin-file-manager'    , os.path.join ('tools', 'batch', 'plugins', 'file-manager')    , 'tools'   , ['libs/batch/plugins/file-manager', 'libs/bus', 'libs/config', 'libs/rpc', 'libs/utils', "libs/convert"])
       , 'tools/batch/plugins/parser'          : HProject   ('batch-plugin-parser'          , os.path.join ('tools', 'batch', 'plugins', 'parser')          , 'tools'   , ['libs/batch/batch', 'libs/bus', 'libs/config', 'libs/luna/core', 'libs/rpc', 'libs/utils', "libs/convert"])
       , 'tools/batch/plugins/plugin-manager'  : HProject   ('batch-plugin-plugin-manager'  , os.path.join ('tools', 'batch', 'plugins', 'plugin-manager')  , 'tools'   , ['libs/bus', 'libs/config', 'libs/rpc', 'libs/utils', "libs/convert"])
       , 'tools/batch/plugins/project-manager' : HProject   ('batch-plugin-project-manager' , os.path.join ('tools', 'batch', 'plugins', 'project-manager') , 'tools'   , ['libs/batch/batch', 'libs/batch/plugins/project-manager', 'libs/bus', 'libs/config', 'libs/luna/core', 'libs/rpc', 'libs/utils', "libs/convert"])
       , 'tools/batch/plugins/s3-file-manager' : HProject   ('batch-plugin-s3-file-manager' , os.path.join ('tools', 'batch', 'plugins', 's3-file-manager') , 'tools'   , ['libs/aws', 'libs/batch/batch', 'libs/batch/plugins/file-manager', 'libs/bus', 'libs/config', 'libs/luna/core', 'libs/rpc', 'libs/utils', "libs/convert"])
       , 'tools/batch/plugins/ur-manager'      : HProject   ('batch-plugin-ur-manager'      , os.path.join ('tools', 'batch', 'plugins', 'ur-manager')      , 'tools'   , ['libs/config', 'libs/rpc', 'libs/bus', 'libs/batch/batch', 'libs/batch/plugins/ur-manager', "libs/convert"])
       , 'tools/initializer'                   : HProject   ('flowbox-initializer-cli'      , os.path.join ('tools', 'initializer')                         , 'tools'   , ['libs/config', 'libs/luna/initializer', 'libs/utils', "libs/convert"])
       , 'tools/lunac'                         : HProject   ('luna-compiler'                , os.path.join ('tools', 'lunac')                               , 'tools'   , ['libs/config', 'libs/luna/build', 'libs/luna/core', 'libs/luna/distribution', 'libs/luna/initializer', 'libs/luna/pass', 'libs/utils', "libs/convert"])
       , 'tools/wrappers'                      : HProject   ('flowbox-wrappers'             , os.path.join ('tools', 'wrappers')                            , 'wrappers', ['libs/config', "libs/convert"])

       , 'third-party/algebraic'               : HProject   ('algebraic'                    , os.path.join ('third-party', 'algebraic')                     , 'third-party', ['third-party/accelerate', "libs/convert"])
       , 'third-party/accelerate'              : HProject   ('accelerate'                   , os.path.join ('third-party', 'accelerate')                    , 'third-party', [])
       , 'third-party/accelerate-cuda'         : HProject   ('accelerate-cuda'              , os.path.join ('third-party', 'accelerate-cuda')               , 'third-party', ['third-party/mainland-pretty'], flags=Flags([Flag('-fdebug')])) # [KL] accelerate debug flag is necessary to dump generated CUDA kernels
       , 'third-party/accelerate-fft'          : HProject   ('accelerate-fft'               , os.path.join ('third-party', 'accelerate-fft')                , 'third-party', [])
       , 'third-party/accelerate-io'           : HProject   ('accelerate-io'                , os.path.join ('third-party', 'accelerate-io')                 , 'third-party', [])
       , 'third-party/binary'                  : HProject   ('binary'                       , os.path.join ('third-party', 'binary')                        , 'third-party', [])
       , 'third-party/fgl'                     : HProject   ('fgl'                          , os.path.join ('third-party', 'fgl')                           , 'third-party', []) # [PM] temporary fix until https://github.com/haskell/fgl/pull/7 is merged
       , 'third-party/HMap'                    : HProject   ('HMap'                         , os.path.join ('third-party', 'HMap')                          , 'third-party', []) # [PM] temporary fix
       , 'third-party/imagemagick'             : HProject   ('imagemagick'                  , os.path.join ('third-party', 'imagemagick')                   , 'third-party', []) # [KL] temporary fix until imagemagick is fixed
       , 'third-party/linear-accelerate'       : HProject   ('linear-accelerate'            , os.path.join ('third-party', 'linear-accelerate')             , 'third-party', ['third-party/accelerate']) # [MM] not so temporary fix, included because of too strict upper bound on accelerate
       , 'third-party/mainland-pretty'         : HProject   ('mainland-pretty'              , os.path.join ('third-party', 'mainland-pretty')               , 'third-party', []) # [MM] temporary fix until mainland-pretty relaxes upper bound on text to allow version 1.2
       , 'third-party/graphviz-2999.17.0.2.2'  : HProject   ('graphviz'                     , os.path.join ('third-party', 'graphviz-2999.17.0.2.2')        , 'third-party', [])
       })
