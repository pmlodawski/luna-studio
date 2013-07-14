###########################################################################
## Copyright (C) Flowbox, Inc # All Rights Reserved
## Unauthorized copying of this file, via any medium is strictly prohibited
## Proprietary and confidential
## Flowbox Team <contact@flowbox.io>, 2013
###########################################################################

import os
import fnmatch

def glob_recursive(pathlist, regex='*.*'):
    paths = [os.path.join(dirpath, f)
             for path in pathlist
             for dirpath, dirnames, files in os.walk(path)
             for f in fnmatch.filter(files, regex)]
    return list(set([os.path.normpath(path) for path in paths]))