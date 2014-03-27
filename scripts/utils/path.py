###########################################################################
## Copyright (C) Flowbox, Inc / All Rights Reserved
## Unauthorized copying of this file, via any medium is strictly prohibited
## Proprietary and confidential
## Flowbox Team <contact@flowbox.io>, 2014
###########################################################################

import os

def write_if_changed(path, s):
    if os.path.exists(path):
        with open(path, 'r') as file:
            if file.read() == s:
                return
    with open(path, 'w') as file:
        file.write(s)