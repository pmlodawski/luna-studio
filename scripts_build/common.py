import contextlib
import os
import subprocess
import sys


@contextlib.contextmanager
def working_directory(path: str):
    """A context manager which changes the working directory to the given
    path, and then changes it back to its previous value on exit.

    """
    prev_cwd = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(prev_cwd)


def fail(msg:str='', exit_code:int=1):
    if msg:
        print(msg)
    print('Status: FAIL')
    sys.exit(exit_code)


@contextlib.contextmanager
def process_context(name: str):
    """Run some operations, wrapping everything in a try/catch
    """
    try:
        yield
    except subprocess.CalledProcessError:
        fail(msg='Error while running {}'.format(name))