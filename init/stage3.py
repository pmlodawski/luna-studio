#!/usr/bin/env python3

# ##### Bootstrapping the repository. Stage 3. #####
# Stage 3: prepare the repository.
#
from contextlib import contextmanager
import logging
import os
import shutil
import stat
import sys
from pathlib import Path
import multiprocessing
import urllib
import urllib.request
import platform

import git
from clint.textui import colored
from plumbum import local
import plumbum

from plumbum.commands.processes import ProcessExecutionError

from ctx_managers import releasing, caveat
from exceptions import ShtackWrongStackVersion
from io_utils import ferror, finfo, fprint, fmt
from log_config import init_logger, main_logger


def bind_git_hooks():
    this_script_p = Path(sys.argv[0])
    hooks_p = this_script_p.parent.parent / 'hooks'

    git_hooks_p = Path(".git", "hooks")

    fprint(colored.blue("INFO: ") + "symlinking '{hooks_p}' to your '{git_hooks_p}'")

    try:
        cav = """
        Caveat: Microsoft thinks you're an ugly, dirty hacker because you want symlinks. zOMG…
                Rerun this script as root or add yourself 'SeCreateSymbolicLinkPrivilege'.
        """
        with caveat(cav, OSError, platforms='Windows'):
            if git_hooks_p.is_dir() or git_hooks_p.is_symlink():
                shutil.rmtree(str(git_hooks_p),
                              onerror=lambda exc_fun, path, exc_ifo: os.unlink(path))
    except OSError as e:
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


def get_submodules():
    finfo("Initialising submodules; if there are changes already, perform (recursive) merge")
    git_cmd = local["git"]
    git_cmd[
        "submodule", "update", "--init"
    ]()
    git_cmd[
        "submodule", "foreach", "-q", "--recursive",
        """branch="$(git config -f $toplevel/.gitmodules submodule.$name.branch)"; git checkout $branch"""
    ]()
    git_cmd[
       "submodule", "foreach", "-q", "--recursive",
       """commsha="$(cd $toplevel; git ls-tree @ $name | awk '{print $3}' )"; git merge $commsha"""
    ]()


def get_stack():

    # customizables

    bootstrapping_stack_version = '0.1.2.0'
    bootstrapping_stack_arch = 'x86_64'
    target_stack_gitsha = '5461d3e071bf4478b40ea9946e4c425f4669c46c'
    number_of_jobs = multiprocessing.cpu_count()

    stack_gitrepo = 'git@github.com:commercialhaskell/stack.git'

    # logic

    try:
        curr_ver = local["stack"]["--version"]()
        curr_ver = curr_ver.split()[4]
        if curr_ver != target_stack_gitsha:
            raise ShtackWrongStackVersion
    except plumbum.CommandNotFound:
        finfo("it seems that you don't have `stack` installed. I'll get it for you")
    except ShtackWrongStackVersion:
        finfo("it seems that you have `stack` installed but it's in wrong version. I'll get it for you")
    else:
        finfo("it seems that you already have `stack` installed and in correct version. Good!")
        return

    this_system = platform.system()
    if this_system == 'Darwin':
        bootstrapping_stack_os_abbrev = 'osx'
    elif this_system == 'Linux':
        bootstrapping_stack_os_abbrev = 'linux'
    else:
        ferror("I can't get `stack` for you, your system '{this_system}' is not supported :(")
        return

    bootstrapping_stack_url = fmt("https://github.com/commercialhaskell/stack/releases/download/"
                                  "v{bootstrapping_stack_version}/stack"
                                  "-{bootstrapping_stack_version}"
                                  "-{bootstrapping_stack_arch}"
                                  "-{bootstrapping_stack_os_abbrev}.gz")

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

    with tempdir_debuggable(root_pth='temp', name='stack', preserve=True) as (tmpdir, already_existed):
        with local.cwd(tmpdir):
            suppress_exception_list = not already_existed and [ProcessExecutionError] or []

            git_cmd = local["git"]

            finfo("cloning stack git")
            with suppress_callback(*suppress_exception_list,
                                   on_caugth_exc=lambda: finfo("There was an error but this could happen. Assuming nothing interesting happened, moving along.")):
                git_cmd["clone", stack_gitrepo]()

            with local.cwd(local.cwd / "stack"):
                git_cmd["checkout", target_stack_gitsha]()

                finfo("downloading bootstrapping stack from {bootstrapping_stack_url}")
                urllib.request.urlretrieve(bootstrapping_stack_url, "stack.gz")

                finfo("extracting & making executable")
                local["gunzip"]["stack.gz"]()

                st = os.stat("./stack")
                os.chmod("./stack", st.st_mode | stat.S_IEXEC)

                finfo("getting GHC if required")
                bootstrapping_stack_cmd = local["./stack"]
                bootstrapping_stack_cmd["setup"]()

                finfo("building proper stack")
                bootstrapping_stack_cmd["build", "-j", str(number_of_jobs)]()

                finfo("copying stack to '{target_stack_localtion_pth}'")
                target_stack_pths = Path(
                    fmt(
                        ".stack-work/install/{bootstrapping_stack_arch}-{bootstrapping_stack_os_abbrev}/")
                    ).glob("lts-*/*/bin/stack")
                target_stack_pths = list(target_stack_pths)
                try:
                    [target_stack_pth] = target_stack_pths
                    shutil.move(str(target_stack_pth),
                                str(target_stack_localtion_pth))

                    finfo("stack is fresh & ready. Put it somewhere in your PATH")
                except ValueError as e:
                    ferror("Glob pattern found more or less than one entry that match.")
                    fprint("Found:")
                    if target_stack_pths:
                        for target_stack_pth in target_stack_pths:
                            fprint("- {target_stack_pth}")
                    else:
                        fprint(" (nothing found)")

                    raise e


@contextmanager
def suppress_callback(*exc, on_caugth_exc=None):
    try:
        yield
    except exc:
        try:
            if on_caugth_exc:
                on_caugth_exc()
        except TypeError as e:
            ferror("There was a problem with callback")
            raise e


@contextmanager
def tempdir_debuggable(*, root_pth, name, preserve=False):
    """Works in a similar way to tempfile.TemporaryDirectory but on exception does NOT remove the dir, only displays
    the path, to allow debugging."""

    name_pth = Path(root_pth) / name

    already_existed = name_pth.is_dir() or name_pth.is_symlink()

    if already_existed:
        if preserve:
            finfo("'{name_pth}' already exists, preseving")
        else:
            finfo("'{name_pth}' already exists, removing")
            shutil.rmtree(str(name_pth),
                          onerror=lambda exc_fun, path, exc_ifo: os.unlink(str(name_pth)))

    else:
        finfo("Creating '{name_pth}'")
        name_pth.mkdir(parents=True)

    try:
        yield str(name_pth), already_existed
    except Exception as e:
        ferror("Not removing temp directory '{name_pth}' since some error occured, please investigate")
        raise e
    else:
        finfo("Removing temp dir '{name_pth}'")
        shutil.rmtree(str(name_pth))


def main():
    try:
        print("##############################" + colored.blue(" STAGE: 3 ") + "##############################")

        init_logger()
        log = main_logger()

        bind_git_hooks()
        configure_repo()
        create_aliases()
        get_submodules()
        get_stack()
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
