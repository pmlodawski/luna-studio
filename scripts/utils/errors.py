###########################################################################
## Copyright (C) Flowbox, Inc / All Rights Reserved
## Unauthorized copying of this file, via any medium is strictly prohibited
## Proprietary and confidential
## Flowbox Team <contact@flowbox.io>, 2014
###########################################################################

import sys
from utils.colors import print_error


class FlowboxError(Exception):
  pass


def fatal():
    raise FlowboxError("Error!")


def handle_out(code):
    if code:
        raise FlowboxError("Execution error (handle_out)! Return code {code} != 0".format(**locals()))

def handle_err(code):
    if code:
        raise FlowboxError("Execution error (handle_err)! Return code {code} != 0".format(**locals()))