#!/usr/bin/env python3

# ##### Bootstrapping the repository. Stage 3. #####
# Stage 3: prepare the repository.
#

from operator import itemgetter
import os
import shutil
from configparser import NoSectionError, NoOptionError
import sys
from pathlib import Path

from io_utils import fprint
from io_utils import fprint, fmt
# noinspection PyUnresolvedReferences
import git
# noinspection PyUnresolvedReferences
from clint.textui import puts, colored
# noinspection PyUnresolvedReferences
from plumbum import local
# noinspection PyUnresolvedReferences
import plumbum


def bind_gitmodules():
    fprint(colored.blue("INFO: ") + "overwriting .gitmodules with _gitmodules")

    with open("_gitmodules", 'r') as config_source:
        with open(".gitmodules", 'w') as config_dest:
            for line in config_source:
                config_dest.write(line)


def update_gitmodules():
    fprint(colored.blue("INFO: ") + "initialising git-modules")
    git_command = local["git"]
    git_command["submodule", "init"]()
    git_command["submodule", "sync", "--recursive"]()
    git_command["submodule", "update", "--recursive"]()


def bind_git_hooks():
    pass


def configure_repo():
    pass


def main():
    try:
        print("##############################" + colored.blue(" STAGE: 3 ") + "##############################")
        bind_gitmodules()
        update_gitmodules()
        bind_git_hooks()
        configure_repo()
    except Exception as e:
        terminal_width = shutil.get_terminal_size((80, 20)).columns
        hash_sign = "#"
        fprint("{hash_sign:#^{terminal_width}}", colour='red')
        print("Stage 3 got exception:")
        raise e

if __name__ == '__main__':
    main()
