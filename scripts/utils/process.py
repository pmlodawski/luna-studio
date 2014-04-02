###########################################################################
## Copyright (C) Flowbox, Inc / All Rights Reserved
## Unauthorized copying of this file, via any medium is strictly prohibited
## Proprietary and confidential
## Flowbox Team <contact@flowbox.io>, 2014
###########################################################################

import sys

from colors import print_error
from subprocess import call



def handle_out(code):
    if code != 0 and code != "":
        print_error("ERROR")
        sys.exit(code)

def autocall(args):
    handle_out(call(args))