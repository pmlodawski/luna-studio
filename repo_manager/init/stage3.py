#!/usr/bin/env python3

# ##### Bootstrapping the repository. Stage 3. #####
# Stage 3: prepare the repository.
#
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

from ctx_managers import releasing, caveat, suppress_callback, tempdir_debuggable
from exceptions import ShtackWrongStackVersion
from io_utils import fmt
from log_config import main_logger, logging_action

if __name__ == '__main__':
    log = main_logger(init=True)
else:
    log = logging.getLogger(__name__)


def bind_git_hooks():
    global log
    with logging_action(log, 'binding git hooks') as logHooks:
        this_script_p = Path(sys.argv[0])
        hooks_p = this_script_p.parent.parent / 'hooks'

        git_hooks_p = Path(".git", "hooks")

        logHooks.info("symlinking '{hooks_p}' to your '{git_hooks_p}'")

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
            logHooks.error("tried to remove current '{git_hooks_p}' but failed.")
            raise e

        # noinspection PyTypeChecker
        git_hooks_p.symlink_to(Path('..') / hooks_p, target_is_directory=True)


def configure_repo():
    # noinspection PyUnresolvedReferences
    this_repo = git.Repo('.')

    with releasing(this_repo.config_writer()) as cfg:
        cfg.set_value("status", "submoduleSummary", "true")
        cfg.set_value("diff", "submodule", "log")
        cfg.set_value("fetch", "recurseSubmodules", "on-demand")


def create_aliases():
    # noinspection PyUnresolvedReferences
    this_repo = git.Repo('.')

    with releasing(this_repo.config_writer()) as cfg:
        cfg.set_value("alias", "commit-all", "!./.git/hooks/_commit-all")
        cfg.set_value("alias", "shtack-update", "!./.git/hooks/_shtack-update")


def get_submodules():
    with logging_action(log, 'preparing submodules') as logSubm:
        logSubm.info("getting submodules")
        git_cmd = local["git"]

        logSubm.debug("initialising; if there are changes already, perform (recursive) merge")
        git_cmd[
            "submodule", "update", "--init"
        ]()

        logSubm.debug("checking out branches")
        git_cmd[
            "submodule", "foreach", "-q", "--recursive",
            """branch="$(git config -f $toplevel/.gitmodules submodule.$name.branch)"; git checkout $branch"""
        ]()

        logSubm.debug("merging with HEAD")
        git_cmd[
            "submodule", "foreach", "-q", "--recursive",
            """commsha="$(cd $toplevel; git ls-tree @ $name | awk '{print $3}' )"; git merge $commsha"""
        ]()


def get_stack():
    with logging_action(log, 'obtaining `stack`') as logStack:

        # customizables

        bootstrapping_stack_version = '0.1.2.0'
        bootstrapping_stack_arch = 'x86_64'
        target_stack_gitsha = '5461d3e071bf4478b40ea9946e4c425f4669c46c'
        number_of_jobs = multiprocessing.cpu_count()

        stack_gitrepo = 'git@github.com:commercialhaskell/stack.git'

        # logic

        try:
            stack_cmd = local["stack"]  # path to stack or None
            stack_ok = True             # is stack avail. and is it ok for our needs?
            curr_ver = stack_cmd["--version"]()
            curr_ver_sha = curr_ver.split()[4]
            if curr_ver_sha != target_stack_gitsha:
                raise ShtackWrongStackVersion
        except plumbum.CommandNotFound:
            logStack.info("it seems that you don't have `stack` installed. I'll get it for you")
            stack_cmd, stack_ok = None, False
        except ShtackWrongStackVersion:
            logStack.info("it seems that you have `stack` installed but it's in wrong version. I'll get it for you")
            logStack.debug("stack reported version: {curr_ver}")
            logStack.debug("stack reported version - extracted SHA1: {curr_ver_sha}")
            stack_ok = False
        else:
            logStack.info("it seems that you already have `stack` installed and in correct version. Good!")
            return

        this_system = platform.system()
        if this_system == 'Darwin':
            bootstrapping_stack_os_abbrev = 'osx'
        elif this_system == 'Linux':
            bootstrapping_stack_os_abbrev = 'linux'
        else:
            logStack.error("I can't get `stack` for you, your system '{this_system}' is not supported :(")
            return

        bootstrapping_stack_url = fmt("https://github.com/commercialhaskell/stack/releases/download/"
                                      "v{bootstrapping_stack_version}/stack"
                                      "-{bootstrapping_stack_version}"
                                      "-{bootstrapping_stack_arch}"
                                      "-{bootstrapping_stack_os_abbrev}.gz")

        logStack.info("""
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
                suppress_exception_list = already_existed and [ProcessExecutionError] or []

                git_cmd = local["git"]

                logStack.info("cloning stack git")
                with suppress_callback(*suppress_exception_list,
                                       on_caugth_exc=lambda: logStack.info("There was an error but this could happen. "
                                                                           "Assuming nothing interesting happened, "
                                                                           "moving on.",
                                                                           exc_info=True)):
                    git_cmd["clone", stack_gitrepo]()

                with local.cwd(local.cwd / "stack"):
                    git_cmd["checkout", target_stack_gitsha]()

                    if stack_ok:
                        logStack.info("using your current `stack`")
                    else:
                        logStack.info("downloading bootstrapping stack from {bootstrapping_stack_url}")
                        urllib.request.urlretrieve(bootstrapping_stack_url, "stack.gz")

                        logStack.info("extracting & making executable")
                        with suppress_callback(ProcessExecutionError,
                                               on_caugth_exc=lambda: logStack.info("Gunzip error, but this could happen"
                                                                                   "because file already exists. "
                                                                                   "Moving on.",
                                                                                   exc_info=True)):
                            local["gunzip"]["stack.gz"]()

                        st = os.stat("./stack")
                        os.chmod("./stack", st.st_mode | stat.S_IEXEC)

                        stack_cmd = local["./stack"]

                    logStack.info("stack is getting GHC for you if required")
                    bootstrapping_stack_cmd = local["./stack"]
                    bootstrapping_stack_cmd["setup"]()

                    stack_build_logfile = "stack_build.log"
                    logStack.info("building proper stack, this can take a while. Build progress: "
                                  "`{local.cwd}/{stack_build_logfile}`")
                    # noinspection PyCallingNonCallable
                    (bootstrapping_stack_cmd["build", "-j", str(number_of_jobs)] > stack_build_logfile)()

                    logStack.info("copying stack to '{target_stack_localtion_pth}'")
                    target_stack_pths = Path(
                        fmt(
                            ".stack-work/install/{bootstrapping_stack_arch}-{bootstrapping_stack_os_abbrev}/")
                        ).glob("lts-*/*/bin/stack")
                    target_stack_pths = list(target_stack_pths)
                    try:
                        [target_stack_pth] = target_stack_pths
                        shutil.move(str(target_stack_pth),
                                    str(target_stack_localtion_pth))

                        logStack.info("stack is fresh & ready! Put it somewhere in your PATH")
                    except ValueError as e:
                        logStack.error("Glob pattern found more or less than one entry that match.")

                        dbg = ["Found:"]
                        if target_stack_pths:
                            for target_stack_pth in target_stack_pths:
                                dbg.append("- {target_stack_pth}")
                        else:
                            dbg.append(" (nothing found)")

                        logStack.debug('\n'.join(dbg))

                        raise e


def main():
    # noinspection PyBroadException
    try:
        print("##############################" + colored.blue(" STAGE: 3 ") + "##############################")
        bind_git_hooks()
        configure_repo()
        create_aliases()
        get_submodules()
        get_stack()
    except Exception as e:
        log.exception("error in initializer stage 3", exc_info=True)
    else:
        log.info("all seems done")

if __name__ == '__main__':
    main()
