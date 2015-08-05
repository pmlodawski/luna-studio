#!/usr/bin/env python3

"""pre-push hook.

Usage:
  pre-push <name> <location>
"""

# noinspection PyUnresolvedReferences
from docopt import docopt
# noinspection PyUnresolvedReferences
import git
from pathlib import Path
import sys
import traceback
from git_utils import releasing
from io_utils import fprint
from shtack_exceptions import ShtackHookAbort


def main():
    try:
        args = docopt(__doc__)
        print("example pre-push: " + str(args))

        this_repo = git.Repo(str(Path('.').resolve()))

        # for remote in this_repo.remotes:
        #     remote.update()

        with releasing(this_repo.config_reader()) as cfg:
            for submodule in this_repo.submodules:
                submod_repo = git.Repo(submodule.name)

                if submod_repo.head.commit.hexsha != submodule.hexsha:
                    fprint("Submodule {submodule} has changes.", colour='yellow')

                if submod_repo.head.ref.tracking_branch().commit.hexsha != submod_repo.head.ref.commit.hexsha:
                    raise ShtackHookAbort("Submodule {submodule} points to commits that are not on remote.")

    except ShtackHookAbort as e:
        fprint(e.args[0], colour='red')
        traceback.print_stack()
        sys.exit(1)


if __name__ == '__main__':
    main()
