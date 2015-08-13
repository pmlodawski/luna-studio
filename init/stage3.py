#!/usr/bin/env python3

# ##### Bootstrapping the repository. Stage 3. #####
# Stage 3: prepare the repository.
#

import os
import shutil
import sys
from pathlib import Path
from git_utils import releasing

from io_utils import fprint, fmt, finfo
# noinspection PyUnresolvedReferences
import git
# noinspection PyUnresolvedReferences
from clint.textui import puts, colored
# noinspection PyUnresolvedReferences
from plumbum import local
# noinspection PyUnresolvedReferences
import plumbum


def bind_gitmodules():
    this_repo = git.Repo('.')

    finfo("""
    assuming that everybody knows that one *needs* ./init_repo.py before doing any
          work. Is that right?
    """)

def update_gitmodules():
    fprint(colored.blue("INFO: ") + "initialising git-modules")
    git_command = local["git"]
    git_command["submodule", "init"]()
    git_command["submodule", "sync", "--recursive"]()
    git_command["submodule", "update", "--recursive"]()


def bind_git_hooks():
    this_script_p = Path(sys.argv[0])
    hooks_p = this_script_p.parent.parent / 'hooks'

    git_hooks_p = Path(".git", "hooks")

    fprint(colored.blue("INFO: ") + "symlinking '{hooks_p}' to your '{git_hooks_p}'")

    try:
        if git_hooks_p.is_dir() or git_hooks_p.is_symlink():
            shutil.rmtree(str(git_hooks_p),
                          onerror=lambda exc_fun, path, exc_ifo: os.unlink(path))
    except OSError as e:
        fprint("""
        Caveat (Windows): Microsoft thinks you're an ugly, dirty hacker because you want symlinks. zOMG…
                          Rerun this script as root or add yourself 'SeCreateSymbolicLinkPrivilege'.
        """, colour='yellow')
        raise Exception(fmt("ERROR: tried to remove current '{git_hooks_p}' but failed.")) from e

    # noinspection PyTypeChecker
    git_hooks_p.symlink_to(Path('..') / hooks_p, target_is_directory=True)


def configure_repo():
    this_repo = git.Repo('.')

    with releasing(this_repo.config_writer()) as cfg:
        cfg.set_value("status", "submoduleSummary", "true")
        cfg.set_value("diff", "submodule", "log")
        cfg.set_value("fetch", "recurseSubmodules", "on-demand")


def create_aliases():
    this_repo = git.Repo('.')

    with releasing(this_repo.config_writer()) as cfg:
        cfg.set_value("alias", "commit-all", "!./.git/hooks/_commit-all")
        cfg.set_value("alias", "shtack-update", "!./.git/hooks/_shtack-update")


def main():
    try:
        print("##############################" + colored.blue(" STAGE: 3 ") + "##############################")
        bind_git_hooks()
        bind_gitmodules()
        update_gitmodules()
        configure_repo()
        create_aliases()
    except Exception as e:
        # noinspection PyUnusedLocal
        terminal_width = shutil.get_terminal_size((80, 20)).columns
        # noinspection PyUnusedLocal
        hash_sign = "#"
        fprint("{hash_sign:#^{terminal_width}}", colour='red')
        print("Stage 3 got exception:")
        raise e
    else:
        print(colored.blue("INFO: ") + "all seems okay")

if __name__ == '__main__':
    main()
