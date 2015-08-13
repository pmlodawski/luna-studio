#!/usr/bin/env python3

# ##### Bootstrapping the repository. Stage 3. #####
# Stage 3: prepare the repository.
#

import os
import platform
import shutil
import stat
import sys
from pathlib import Path
import multiprocessing
import tempfile
import urllib.request
from git_utils import releasing

from io_utils import fprint, fmt, ferror, finfo
# noinspection PyUnresolvedReferences
import git
# noinspection PyUnresolvedReferences
from clint.textui import puts, colored
# noinspection PyUnresolvedReferences
from plumbum import local
# noinspection PyUnresolvedReferences
import plumbum


def bind_git_hooks():
    this_script_p = Path(sys.argv[0])  # making conflicts!
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


def get_stack():
    # try:
    #     local["stack"]["--version"]()
    # except plumbum.CommandNotFound:
    #     finfo("it seems that you don't have `stack` installed. I'll get it for you")
    # else:
    #     finfo("it seems that you already have `stack` installed. Good!")
    #     return

    this_system = platform.system()
    if this_system == 'Darwin':
        bootstrapping_stack_os_abbrev = 'osx'
    elif this_system == 'Linux':
        bootstrapping_stack_os_abbrev = 'linux'
    else:
        ferror("I can't get `stack` for you, your system '{this_system}' is not supported :(")
        return

    bootstrapping_stack_version = '0.1.2.0'
    bootstrapping_stack_arch = 'x86_64'
    target_stack_gitsha = 'c5b98565e0401453ba3c86c01a41db64dff5b69c'
    number_of_jobs = multiprocessing.cpu_count()

    bootstrapping_stack_url = fmt("https://github.com/commercialhaskell/stack/releases/download/"
                                  "v{bootstrapping_stack_version}/stack"
                                  "-{bootstrapping_stack_version}"
                                  "-{bootstrapping_stack_arch}"
                                  "-{bootstrapping_stack_os_abbrev}.gz")

    stack_gitrepo = 'git@github.com:commercialhaskell/stack.git'

    fprint("""
    To give you `stack`, I will get the following for you (in order):
      - download stack used to build stack:
        version:     {bootstrapping_stack_version}
        arch:        {bootstrapping_stack_arch}
        OS:          {bootstrapping_stack_os_abbrev}
      - build proper stack with stack:
        git-version: {target_stack_gitsha}
        build jobs:  {number_of_jobs}
    """)

    target_stack_localtion_pth = local.cwd / "stack"

    with tempfile.TemporaryDirectory(prefix="flowbox_shtack_") as tmpdir:
        with local.cwd(tmpdir):
            finfo("cloning stack git")
            git_cmd = local["git"]
            git_cmd["clone", stack_gitrepo]()

            with local.cwd(local.cwd / "stack"):
                git_cmd["checkout", target_stack_gitsha]()

                finfo("downloading bootstrapping stack from {bootstrapping_stack_url}")
                urllib.request.urlretrieve(bootstrapping_stack_url, "stack.gz")

                finfo("extracting & making executable")
                local["gunzip"]["stack.gz"]()
                bootstrapping_stack_pth = Path("./stack")
                st = os.stat(str(bootstrapping_stack_pth))
                os.chmod(str(bootstrapping_stack_pth), st.st_mode | stat.S_IEXEC)

                finfo("getting GHC if required")
                bootstrapping_stack_cmd = local[str(bootstrapping_stack_pth)]
                bootstrapping_stack_cmd["setup"]()

                finfo("building proper stack")
                bootstrapping_stack_cmd["build", "-j", str(number_of_jobs)]()

                finfo("copying stack to '{target_stack_localtion_pth}'")
                [target_stack_pth] = Path(fmt(".stack-work/install/{bootstrapping_stack_arch}-{bootstrapping_stack_os_abbrev}/lts-2.17/"))\
                    .glob("*/bin/stack")
                shutil.move(str(target_stack_pth), str(target_stack_localtion_pth))

                finfo("stack is fresh & ready. Put it somewhere in your PATH")


def main():
    try:
        print("##############################" + colored.blue(" STAGE: 3 ") + "##############################")
        bind_git_hooks()
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
