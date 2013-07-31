#!/usr/bin/env python

import os
import sys
import glob

# Compiler path will be always one directory above the script directory.
# This ignores current working directory... but should be considered a feature. 
compilerRootDir = os.path.dirname(os.path.dirname(os.path.realpath(__file__))) + "/"

# Folders with .thrift files (input)
commonFilesLocation = "tools/lunac/thrift/"
batchFilesLocation = "tools/batch/thrift/"

# Output folders for thrift generation
hsOutput = "tools/batch/"
cppOutput = "tools/batch-client/"

def processFile(file, dir, language):
    print "thrift -gen " + language + " -o " + dir + " " + file
    os.system("thrift -gen " + language + " -o " + dir + " " + file)

def processDirectory(dir):
    for file in glob.glob(compilerRootDir + dir + "/*.thrift"):
        processFile(file, compilerRootDir + hsOutput, "hs")
        processFile(file, compilerRootDir + cppOutput, "cpp")

processDirectory(commonFilesLocation)
processDirectory(batchFilesLocation)