#!/usr/bin/env python3

# ##### Bootstrapping the repository. Stage 3. #####
# Stage 3: prepare the repository.
#
import shutil
from gitlord.io_utils import fprint


def bind_gitmodules():
    pass


def update_gitmodules():
    pass


def bind_git_hooks():
    pass


def configure_repo():
    pass


def main():
    try:
        bind_gitmodules()
        update_gitmodules()
        bind_git_hooks()
        configure_repo()
    except Exception as e:
        terminal_width = shutil.get_terminal_size((80, 20)).columns
        hash_sign = "#"
        fprint("{hash_sign:#^{terminal_width}}", colour='cyan')
        print("Stage 3 got exception:")
        raise e

if __name__ == '__main__':
    main()
