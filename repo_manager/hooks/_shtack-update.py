#!/usr/bin/env python3

# noinspection PyUnresolvedReferences
from plumbum import local


def main():
    with local.cwd(local.cwd / "repo_manager"):
        git_cmd = local["git"]
        git_cmd["pull", "origin", "master"]()

if __name__ == '__main__':
    main()
