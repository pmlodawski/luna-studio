#!/usr/bin/env python
# coding=utf-8

# ##### Bootstrapping the repository. Stage 1. #####
# Stage 1: run in alien environment, prepare virtualenv and call stage 2 within that.
#
# This script's sole objective is to ensure proper Python version & create/initialize virtualenv. This shall be
# idempotent, so this shall be called only once (but more shouldn't hurt).
#
# Please note that this script is intended to work on both py2 and py3.
#

from __future__ import print_function
import os
import subprocess
import sys
import re
import platform


#                   __ _
#                  / _(_)
#   ___ ___  _ __ | |_ _  __ _
#  / __/ _ \| '_ \|  _| |/ _` |
# | (_| (_) | | | | | | | (_| |
#  \___\___/|_| |_|_| |_|\__, |
#                         __/ |
#                        |___/
import shutil


FLOWBOX_GIT = os.path.join(".git", "flowbox")
PYENV = os.path.join(FLOWBOX_GIT, "pyenv")
PYENV_BIN_PYTHON = os.path.join(PYENV, "bin", "python")


required_executables = {
    "git": {
        'msg': "Bro, do you even git?",
        'result': None
    },
    "pip": {
        'msg': "It comes with some (most) Python distributions. "
               "Install with you distro's tools or with 'easy_install pip'.",
        'result': None
    },
    "virtualenv": {
        'msg': "This is a common package of Python. Some Python distributions supply it. "
               "Can be installed with 'pip install virtualenv'.",
        'result': None
    }
}

required_executables_fuzzy = {
    "python3": {
        'regex': "python3.\d$",
        'msg': "While this script does well on Python 2.x, "
               "other scripts requires Python 3.3 or greater.",
        'result': None,
        'check': lambda execname: subprocess.check_output([execname, "-V"], universal_newlines=True) > "Python 3.3."
    }
}


#  _              _
# | |            | |
# | |_ ___   ___ | |___
# | __/ _ \ / _ \| / __|
# | || (_) | (_) | \__ \
#  \__\___/ \___/|_|___/
#


def which(program):
    """Search for program in PATH."""
    # noinspection PyShadowingNames
    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

    fpath, fname = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            path = path.strip('"')
            exe_file = os.path.join(path, program)
            if is_exe(exe_file):
                return exe_file

    return None


def fuzzy_which(program):
    """Search for program in PATH by given regex."""
    candidates = [(file, directory)
                  for directory in os.environ["PATH"].split(os.pathsep) if os.path.isdir(directory)
                  for file in os.listdir(directory) if re.match(program, file, re.VERBOSE)]
    candidates = sorted(candidates, reverse=True)
    candidates = [os.path.join(directory, file) for file, directory in candidates]
    candidates = [
        e
        for i, e in enumerate(candidates)
        if candidates.index(e) == i
    ]
    return candidates


def call(cmd, *args, **kwargs):
    """Call the command, print the INFO to stdout."""
    if args or kwargs:
        print("INFO: calling '%s' with args=%s kwargs=%s" % (cmd, str(args), str(kwargs)))
    else:
        print("INFO: >>> calling '%s'" % (cmd, ))
    ret_code = subprocess.call(cmd, *args, **kwargs)
    print("      <<< '%s' returned: %d" % (cmd, ret_code))
    return ret_code


#      _
#     | |
#  ___| |_ ___ _ __  ___
# / __| __/ _ \ '_ \/ __|
# \__ \ ||  __/ |_) \__ \
# |___/\__\___| .__/|___/
#             | |
#             |_|


current_system = platform.system()
this_is_windows = current_system == 'Windows' or current_system.startswith("CYGWIN_NT")


def stage1_verify_programs():
    """Verifies that all required applications are available."""

    for executable in required_executables:
        required_executables[executable]['result'] = which(executable)

    exec_locations = [required_executables[executable]['result']
                      for executable in required_executables]
    for which_result, executable in zip(exec_locations, required_executables):
        if which_result:
            print("INFO: '%s' -> '%s'." % (executable, which_result))
        else:
            print("ERROR: '%s' was not found in your PATH." % (executable, ), file=sys.stderr)
            additional_msg = required_executables[executable]
            if additional_msg:
                print("       %s" % (additional_msg, ), file=sys.stderr)

    if not all(exec_locations):
        raise Exception("Not all prerequisites met. Satisfy them and re-run this script.")

    print("INFO: stage 1 passed")


def stage2_verify_programs_fuzzy():

    for executable in required_executables_fuzzy:
        required_executables_fuzzy[executable]['result'] = fuzzy_which(required_executables_fuzzy[executable]['regex'])

    exec_candidates = [required_executables_fuzzy[executable]['result']
                       for executable in required_executables_fuzzy]
    for fuzzy_which_results, executable in zip(exec_candidates, required_executables_fuzzy):
        if fuzzy_which_results:
            print("INFO: '%s' query was successful, "
                  "candidates are: %s." % (executable, ', '.join("'%s'" % (x, ) for x in fuzzy_which_results), ))
            required_executables_fuzzy[executable]['result'] = None
            for candidate in fuzzy_which_results:
                try:
                    # noinspection PyCallingNonCallable
                    if required_executables_fuzzy[executable]['check'](candidate):
                        print("      '%s' met requirements, selecting" % (candidate, ))
                        required_executables_fuzzy[executable]['result'] = candidate
                        break
                    else:
                        print("      '%s' didn't met requrements")
                except TypeError:
                    pass
            else:
                print("ERROR: none of the candidates for '%s' met the additional conditions." % (executable, ))
                additional_msg = required_executables_fuzzy[executable]
                if additional_msg:
                    print("       %s" % (additional_msg, ), file=sys.stderr)
        else:
            print("ERROR: '%s' was not found in your PATH." % (executable, ), file=sys.stderr)
            additional_msg = required_executables_fuzzy[executable]
            if additional_msg:
                print("       %s" % (additional_msg, ), file=sys.stderr)

    if not all(required_executables_fuzzy[executable]['result']
               for executable in required_executables_fuzzy):
        raise Exception("Not all prerequisites met. Satisfy them and re-run this script.")

    print("INFO: stage 2 passed")


def stage3_create_virtualenv():
    try:
        print("INFO: ensuring repo-local flowbox directory '%s' exists" % (FLOWBOX_GIT, ))
        os.makedirs(FLOWBOX_GIT)
    except OSError:
        pass

    if os.path.isfile(PYENV_BIN_PYTHON):
        # noinspection PyCallingNonCallable
        if not required_executables_fuzzy["python3"]['check'](PYENV_BIN_PYTHON):
            raise Exception("Whoops, something is not right. '%s' does not "
                            "pass the self-tests. Please, verify that" % (PYENV_BIN_PYTHON, ))

        print("INFO: looks like Python virtualenv is already there, all looks ok")
    else:
        print("INFO: looks like Python virtualenv is not there yet, allow me to make it")
        selected_python = required_executables_fuzzy["python3"]['result']

        if os.path.exists(PYENV):
            try:
                shutil.rmtree(PYENV)
            except OSError:
                try:
                    os.remove(PYENV)
                except OSError:
                    raise Exception("ERROR: could not remove '%s'. Clean that up and restart" % (PYENV, ))

        call("virtualenv --no-site-packages -p '%s' %s" % (selected_python, PYENV), shell=True)


def stage4_initial_gitmodules_update():
    print("INFO: bootstrapping git-modules: obtaining repo-management scripts")
    subprocess.check_call(["git", "submodule", "update", "--init"])


def stage5_jump_to_virtualenv():
    python_in_env = PYENV_BIN_PYTHON
    script_to_call = sys.argv[0][:-4] + "2.py"  # run the second script
    subprocess.check_call([python_in_env, script_to_call])


def main():
    try:
        print('############################## STAGE: 1 ##############################')
        stage1_verify_programs()
        stage2_verify_programs_fuzzy()
        stage3_create_virtualenv()
        stage4_initial_gitmodules_update()
        stage5_jump_to_virtualenv()
    except Exception as e:  # TODO: not compatible with Python 2.5 :<
        print("######################################################################")
        print("Stage 1 got exception:")
        raise e


if __name__ == '__main__':
    main()
