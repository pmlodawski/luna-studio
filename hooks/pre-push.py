#!/usr/bin/env python3

# noinspection PyUnresolvedReferences
import git
from git_utils import releasing
from io_utils import fwarning
from shtack_exceptions import ShtackHookAbort


def main():
    this_repo = git.Repo('.')

    for remote in this_repo.remotes:
        remote.update()

    with releasing(this_repo.config_reader()) as cfg:
        for submodule in this_repo.submodules:
            submod_repo = git.Repo(submodule.name)

            if submod_repo.head.commit.hexsha != submodule.hexsha:
                fwarning("""
                Submodule {submodule} has changes.
                  Submodule repo HEAD: {submod_repo.head.commit.hexsha}
                  Submodule points to: {submodule.hexsha}
                """)

            if submod_repo.head.ref.tracking_branch().commit.hexsha != submodule.hexsha:
                tracking_branch = submod_repo.head.ref.tracking_branch()
                tracking_branch_headref = str(submod_repo.head.ref)
                tracking_branch_str = str(tracking_branch)
                raise ShtackHookAbort("""
                Submodule {submodule} points to commit that's different from remote's branch.
                  Submodule branch                : {submod_repo.head.ref}
                  Submodule remote tracking branch: {tracking_branch}
                    {tracking_branch_headref:<15} -> {submodule.hexsha}
                    {tracking_branch_str:<15} -> {tracking_branch.commit.hexsha}
                """)  # exception message

if __name__ == '__main__':
    main()
