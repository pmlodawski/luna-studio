#!/usr/bin/env python3

# noinspection PyUnresolvedReferences
import git
import os
import subprocess
import tempfile
from pathlib import Path
from git_utils import releasing
from io_utils import getinfo, fprint, finfo, fmt
from shtack_exceptions import ShtackHookAbort


def main():
    this_repo = git.Repo('.')

    submodules_to_commit = []

    for submodule in this_repo.submodules:
        submod_repo = git.Repo(submodule.name)
        if submod_repo.is_dirty():
            submodules_to_commit.append(submodule)
        elif submod_repo.is_dirty(untracked_files=True):
            finfo("There are some untracked changes in {submodule}, but will not commit it")

    for submodule in submodules_to_commit:
        finfo("Will commit in {submodule}")

    if not submodules_to_commit:
        finfo("No submodules have changes to commit")

    # Select editor similarily as git-commit(1) does.
    editor = os.environ.get('GIT_EDITOR', None)
    if not editor:
        with releasing(this_repo.config_reader()) as cfg:
            editor = cfg.get_value('core', 'editor', default=None)
    if not editor:
        editor = os.environ.get('VISUAL', None)
    if not editor:
        editor = os.environ.get('EDITOR', None)
    if not editor:
        editor = 'vi'
    finfo("Selected {editor} to edit message")

    initial_message = """
    multi:
    #  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    #  1st line shouldn't be longer than 50 characters
    #
    # Coordinated multi-commit for git-submodules:
    #
    # - """ + '\n    # - '.join(str(x) for x in submodules_to_commit) + """
    #
    #  Subsequent lines shan't be longer than 72 characters, except for some
    #  parts that shouldn't be formatted, like error messages or code
    #  listings.
    #  _____________________________________________________________________
    """

    message = user_edit(fmt(initial_message),
                        editor=editor)

    # for submodule in submodules_to_commit:
    #     submod_repo = git.Repo(submodule.name)
    #     # TODO: add the thing
    submodule_commits = [git.Repo(submodule.name).index.commit(message) for submodule in submodules_to_commit]

    message_suffix = """

    Changed submodules:

    - """ + '\n    - '.join(fmt("{submodule} @ {comm.hexsha}") for submodule, comm in zip(submodules_to_commit, submodule_commits) )

    this_message = user_edit(message + fmt(message_suffix),
                             editor=editor)

    this_repo.index.commit(this_message)


def user_edit(initial, *, editor):
    commit_file = Path('.git', "FLOWBOX_SHTACK_COMMIT")
    with commit_file.open(mode='w') as fh:
        fh.write(initial)

    try:
        subprocess.call([editor, fh.name])
    except subprocess.CalledProcessError as e:
        raise ShtackHookAbort("""
        The EDITOR ({editor}) quit with error code {e.returncode}.
        """)

    with commit_file.open() as fh:
        message = fh.read()

    if all(not x or x.startswith('#')
           for x in (x.strip()
                     for x in message.splitlines())):
        raise ShtackHookAbort("""
        Empty commit message.
        """)

    return message


if __name__ == '__main__':
    main()
