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
from git_utils import releasing
from io_utils import fprint, fmt


def main():
    args = docopt(__doc__)
    print("example pre-push: " + str(args))

    this_repo = git.Repo(str(Path('.').resolve()))

    # for remote in this_repo.remotes:
    #     remote.update()

    with releasing(this_repo.config_reader()) as cfg:
        for submodule in this_repo.submodules:
            fprint("submod: submodule.name = {submodule.name}", colour='blue')
            fprint("        " + "submodule.hexsha = {submodule.hexsha}")
            fprint("        " + "submodule.branch = {submodule.branch}")
            fprint("        " + "submodule.parent_commit = {submodule.parent_commit}")
            fprint("        " + "submodule.branch_path = {submodule.branch_path}")

            submod_repo = git.Repo(submodule.name)
            fprint("        " + "submod_repo.head = {submod_repo.head}")
            fprint("        " + "submod_repo.head.commit = {submod_repo.head.commit}")
            fprint("        " + "submod_repo.head.reference = {submod_repo.head.reference}")

            if submod_repo.head.commit.hexsha != submodule.hexsha:
                raise Exception("nope")

if __name__ == '__main__':
    main()
