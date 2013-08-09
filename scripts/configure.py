#!/usr/bin/python
import os.path
import sys
from subprocess import call, Popen, PIPE

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


