#!/usr/bin/env python
# coding=utf-8

# ##### Bootstrapping the repository. Stage 0. #####
# Stage 0: call bootstrapping scripts.
#
# To run bootstrapping scripts from repo-management repository, performs the (first) update
# of git-modules.
#
# Please note that this script is intended to work on both py2 and py3.
#


from __future__ import print_function
import os
import subprocess

SUBMODULE_LOCATION = "repo_manager"
STEP1_LOCATION = os.path.join(SUBMODULE_LOCATION, "init", "stage1.py")

def main():
    print('############################## STAGE: 0 ##############################')
    print("INFO: bootstrapping gitmodules to obtain repo-management scripts")
    subprocess.check_call([
        "git", "submodule", "update", "--init"
    ])
    subprocess.check_call([
        "git", "submodule", "foreach", "-q", "--recursive",
        'branch="$(git config -f $toplevel/.gitmodules submodule.$name.branch)"; git checkout $branch'
    ])
    print("INFO: all seems okay")
    subprocess.call(["python", STEP1_LOCATION])  # drop exceptions, since those scripts are already quite verbose


if __name__ == '__main__':
    main()

