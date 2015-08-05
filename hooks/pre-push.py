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


def main():
    args = docopt(__doc__)
    print("example pre-push: " + str(args))

    this_repo = git.Repo(str(Path('.').resolve()))
    for remote in this_repo.remotes:
        remote.update()

if __name__ == '__main__':
    main()