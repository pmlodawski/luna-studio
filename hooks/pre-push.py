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
            # fprint("submod: submodule.name = {submodule.name}", colour='blue')
            # tmp = type(submodule.hexsha)
            # fprint("        " + "submodule.hexsha = {submodule.hexsha} :: {tmp}")
            # tmp = type(submodule.branch)
            # fprint("        " + "submodule.branch = {submodule.branch} :: {tmp}")
            # tmp = type(submodule.branch_path)
            # fprint("        " + "submodule.branch_path = {submodule.branch_path} :: {tmp}")

            submod_repo = git.Repo(submodule.name)
            # fprint("        " + "submod_repo.head = {submod_repo.head}")
            # fprint("        " + "submod_repo.head.commit = {submod_repo.head.commit}")
            # fprint("        " + "submod_repo.head.reference = {submod_repo.head.reference}")
            #
            # getinfo(submod_repo.head.ref.commit)
            # getinfo(submod_repo.head.ref.tracking_branch().commit)

            if submod_repo.head.commit.hexsha != submodule.hexsha:
                fprint("Submodule {submodule} has changes.", colour='yellow')
            if submod_repo.head.ref.tracking_branch().commit.hexsha != submod_repo.head.ref.commit.hexsha:
                fprint("Submodule {submodule} points to commits that are not on remote.", colour='red')
                raise Exception("nope")



def getinfo(ob):
    ob_s = str(ob)
    tob = type(ob).__name__
    fprint("{ob_s:<32} :: {tob}", colour='blue')
    for dob_e in dir(ob):
        if dob_e.startswith("_"):
            pass
        else:
            try:
                dob_eo = getattr(ob, dob_e)
                dob_et = type(dob_eo).__name__
            except:
                dob_et = "???"
            if dob_et == 'method':
                fprint("  > {dob_e:30} :: {dob_et:15}")
            else:
                fprint("  > {dob_e:30} :: {dob_et:15} = {dob_eo}")


if __name__ == '__main__':
    main()
