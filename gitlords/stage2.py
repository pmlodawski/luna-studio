#!/usr/bin/env python3

# ##### Bootstrapping the repository. Stage 2. #####
# Stage 2: run in virtualenv, install required libraries.
#
# This script's sole objective is to prepare virtualenv for further work.
#

import importlib
import shutil
import subprocess

from gitlord.io_utils import fprint


def yield_unless_installed(pkg_name, *, import_name=None, descr=""):
    if not import_name:
        import_name = pkg_name

    try:
        importlib.import_module(import_name)
    except ImportError:
        yield pkg_name, descr, import_name


def get_installation_list():
    yield from yield_unless_installed('clint',
                                      descr="Clint is this library to make a world colorful.")
    yield from yield_unless_installed('docopt',
                                      descr="Because docopt > ArgumentParser.")
    yield from yield_unless_installed('pyyaml',
                                      import_name='yaml',
                                      descr="Required for cooperating with `stack` tool.")
    yield from yield_unless_installed('logbook',
                                      descr="Docbook allows better logging. And this often gets handy…")
    yield from yield_unless_installed('plumbum',
                                      descr="Nice interfacing with OS commands.")


# noinspection PyUnusedLocal
def install_requirements():
    terminal_width = shutil.get_terminal_size((80, 20)).columns
    hash_sign = "#"

    installation_list = list(get_installation_list())

    if installation_list:
        fprint("{hash_sign:#^{terminal_width}}", colour='cyan')
        fprint("# Installation list: ", colour='cyan')
        for pkg, descr, import_name in installation_list:
            fprint("# - {pkg:<15} - {descr}")
        fprint("{hash_sign:#^{terminal_width}}", colour='cyan')

        pip_cmd = ['install']
        pip_cmd.extend(pkg for pkg, descr, import_name in installation_list)

        # noinspection PyUnresolvedReferences
        try:
            # noinspection PyUnresolvedReferences
            import pip
        except ImportError as e:
            raise Exception("No `pip` available. Strange.") from e

        try:
            pip_res = pip.main(pip_cmd)
            fprint("pip_res = {pip_res}")
        except Exception as e:
            raise Exception("Some error occured during installation. Strange.") from e

        fprint("""
        Modules installed, though checking if it was successful is very buggy. "Buggy" as in the sentence
        "there is some race condition, once in a while the same program is okay, but more often it yields
        exceptions".
        Assuming all is okay then.
        """)


def jump_to_next_stage():
    python_in_env = ".git/flowbox/pyenv/bin/python"
    script_to_call = "stage3.py"
    subprocess.check_call([python_in_env, script_to_call])


def main():
    try:
        install_requirements()
        jump_to_next_stage()
    except Exception as e:
        # noinspection PyBroadException
        try:
            terminal_width = shutil.get_terminal_size((80, 20)).columns
            hash_sign = "#"
            fprint("{hash_sign:#^{terminal_width}}", colour='cyan')
        except:
            print("######################################################################")
        print("Stage 2 got exception:")
        raise e


if __name__ == '__main__':
    main()

