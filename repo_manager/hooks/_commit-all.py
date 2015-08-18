#!/usr/bin/env python3

# noinspection PyUnresolvedReferences
import git
import os
import subprocess
from pathlib import Path
from git_utils import releasing
from io_utils import finfo, fmt
from shtack_exceptions import ShtackHookAbort
# noinspection PyUnresolvedReferences
from plumbum import local


def main():
    this_repo = git.Repo('.')

    submodules_to_commit = []

    for submodule in this_repo.submodules:
        submod_repo = git.Repo(submodule.path)
        if submod_repo.index.diff('HEAD'):
            finfo("Will commit {submodule}")
            submodules_to_commit.append(submodule)
        if submod_repo.index.diff(None):
            finfo("There are some untracked changes in {submodule}")

    if not submodules_to_commit:

        finfo("No submodules have changes to commit")

    else:

        editor = get_editor()
        finfo("Selected {editor} to edit message")

        initial_message = """
        multi:
        #  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        #  1st line shouldn't be longer than 50 characters

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

        submodule_commits = [
            git.Repo(submodule.name).index.commit(message)
            for submodule in submodules_to_commit
        ]
        for submodule in submodules_to_commit:
            git_cmd = local["git"]
            git_cmd["add", submodule.path]()
            # this_repo.index.add([submodule])

        message_suffix = """

        Changed submodules:

        - """ + '\n    - '.join(fmt("{submodule} @ {comm.hexsha}") for submodule, comm in zip(submodules_to_commit, submodule_commits) )

        this_message = user_edit(message + fmt(message_suffix),
                                 editor=editor)

        this_repo.index.commit(this_message)


def get_editor():
    """Select editor similarily as git-commit(1) does."""
    editor = os.environ.get('GIT_EDITOR', None)
    if not editor:
        this_repo = git.Repo('.')
        with releasing(this_repo.config_reader()) as cfg:
            try:
                editor = cfg.get_value('core', 'editor', default=None)
            except:  # TODO kgadek: potencjalnie to jest issue w pythongit,
                     # configparser.NoOptionError poleciał
                editor = None
    if not editor:
        editor = os.environ.get('VISUAL', None)
    if not editor:
        editor = os.environ.get('EDITOR', None)
    if not editor:
        editor = 'vi'
    return editor


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

    message = [
        line
        for line in message.splitlines()
        if not line.startswith('#')
    ]

    if all(not x.strip() for x in message):
        raise ShtackHookAbort("Empty commit message.")

    return '\n'.join(message)


if __name__ == '__main__':
    main()
