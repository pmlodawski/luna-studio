#!/usr/bin/env python3

# noinspection PyUnresolvedReferences
import git
from io_utils import fwarning, finfo
from exceptions import ShtackHookAbort


def main():
    this_repo = git.Repo('.')

    finfo("Verifying submodules")
    for submodule in this_repo.submodules:
        if submodule.name.startswith("third-party"):
            continue

        submod_repo = git.Repo(submodule.name)

        finfo("  …checking {submodule}", notrunc=True)

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
