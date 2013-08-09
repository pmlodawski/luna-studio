#!/usr/bin/python
import os.path
import sys
from subprocess import call, Popen, PIPE

options    = ['all', 'luna', 'batch']
opts_all   = options
opts_luna  = ['all', 'luna']
opts_batch = ['all', 'batch']

def wrong_use():
    print_usage()
    sys.exit()

def print_usage():
    print "Usage: compile {%s}" % ", ".join(options)


def info(s):
    print
    print "=== %s ===" % s


def main():
    try:    cmd = sys.argv[1]
    except: wrong_use()
    if cmd not in options:
        print "Unrecognized option '%s'" % cmd
        wrong_use()

    dir = os.path.join(os.path.dirname(os.path.realpath(__file__)), '..')
    p_luna     = os.path.join(dir, 'libs',  'luna')
    p_batch    = os.path.join(dir, 'libs',  'batch')
    p_batchsrv = os.path.join(dir, 'tools', 'batch-srv')
    p_lunac    = os.path.join(dir, 'tools', 'lunac')

    if cmd in opts_luna + opts_batch:
        info("Compiling luna library")
        if call(['cabal-dev', 'install', p_luna, '--force-reinstalls']):
            print "ERROR"
            sys.exit()

    if cmd in opts_batch:
        info("Compiling batch library")
        if call(['cabal-dev', 'install', p_batch, '--force-reinstalls']):
            print "ERROR"
            sys.exit()

    if cmd in opts_batch:
        info("Compiling batchsrv")
        if call(['cabal-dev', 'install', p_batchsrv, '--force-reinstalls']):
            print "ERROR"
            sys.exit()

    if cmd in opts_luna:
        info("Compiling lunac")
        if call(['cabal-dev', 'install', p_lunac, '--force-reinstalls']):
            print "ERROR"
            sys.exit()


main()