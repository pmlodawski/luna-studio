#!/usr/bin/env python3

# noinspection PyUnresolvedReferences
import git


def main():
    this_repo = git.Repo('.')
    this_repo.submodule_update(recursive=True)


if __name__ == '__main__':
    main()
