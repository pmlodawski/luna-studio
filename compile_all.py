#!/usr/bin/python
import os.path
import sys
from subprocess import call, Popen, PIPE

dir = os.path.dirname(os.path.realpath(__file__))
third_party = os.path.join(dir, 'third-party')
p_thrift   = os.path.join(third_party,  'thrift-0.9')
p_missingh = os.path.join(third_party,  'MissingH-1.2.0.0-winpatch')
p_luna     = os.path.join(dir, 'libs',  'luna')
p_batch    = os.path.join(dir, 'libs',  'batch')
p_batchsrv = os.path.join(dir, 'tools', 'batch-srv')
p_lunac    = os.path.join(dir, 'tools', 'lunac')


def check(name):
    print "Checking if '%s' is installed" % name
    (out, err) = Popen(name, stdout=PIPE, shell=True).communicate()
    if not out:
        print "Please install '%s' to continue" % name
        sys.exit()  

check('cabal-dev')

print "Updateing cabal package cache"
if call(['cabal-dev', 'update']):
    print "ERROR"
    sys.exit()

print "Registering thrift library"
if call(['cabal-dev', 'add-source', p_thrift]):
    print "ERROR"
    sys.exit()
	
print "Registering MissingH library (to work on windows)"
if call(['cabal-dev', 'install', p_thrift]):
    print "ERROR"
    sys.exit()

print "Compiling luna"
if call(['cabal-dev', 'install', p_luna, '--force-reinstalls']):
    print "ERROR"
    sys.exit()

print "Compiling batch"
if call(['cabal-dev', 'install', p_batch, '--force-reinstalls']):
    print "ERROR"
    sys.exit()

print "Compiling batchsrv"
if call(['cabal-dev', 'install', p_batchsrv, '--force-reinstalls']):
    print "ERROR"
    sys.exit()

print "Compiling lunac"
if call(['cabal-dev', 'install', p_lunac, '--force-reinstalls']):
    print "ERROR"
    sys.exit()

