#!/usr/bin/env python3

# ##### Bootstrapping the repository. Stage 2: verification. #####
# Stage 2 verification: in new interpreter, try to import freshly installed libraries.
#
import shutil
import sys

from io_utils import fprint


def main():
    try:
        # noinspection PyUnusedLocal
        for name in sys.argv[1:]:
            fprint("Importing {name}...")

    except Exception as e:
        # noinspection PyBroadException
        try:
            # noinspection PyUnusedLocal
            terminal_width = shutil.get_terminal_size((80, 20)).columns
            # noinspection PyUnusedLocal
            hash_sign = "#"
            fprint("{hash_sign:#^{terminal_width}}", colour='cyan')
        except:
            print("######################################################################")
        print("Stage 2-postverify got exception:")
        raise e

if __name__ == '__main__':
    main()
