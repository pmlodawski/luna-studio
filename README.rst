===========================
 New Byte Order repository
===========================

:Date: 2015-08-16 21:31 (Sun, 16 August 2015)
.. date +"%Y-%m-%d %H:%M %z (%a, %d %B %Y)" | pbcopy -
.. import datetime; datetime.datetime.now().strftime("%Y-%m-%d %H:%M %z (%a, %d %B %Y)")

.. contents::


Quickstart
==========

TL;DR
-----

To start using the repository, run ``./init_repo.py`` script with reasonably fresh Python 2 or any Python 3
(i.e. Python 2.5 does not work). It shall finish without errors or display clear message stating what problems
occured and how to fix them.

Scripts
^^^^^^^

``git commit-all``
""""""""""""""""""

Overall:

1. get a list of all submodules, where changes were indexed with ``git add``,
2. open editor, ask for commit message,
3. use commit message for all submodules from stage (1),
4. open editor with previous commit message amended with additional information,
5. commit in the main repository.
   
``git shtack-update``
""""""""""""""""""

Updates the toolset.

Hooks
^^^^^

``post-checkout``
"""""""""""""""""

Ensures that after branch change, every submodules state corresponds to that of the git repo.

``pre-push``
""""""""""""

Simple tool that prevents 90% of the problems with submodules. Prevents one from doing ``git push`` in main
repository when:

1. any of the submodules has fresh commits registered in the main repo,
2. those commits are not present on the remote.
   
The tool shall say what to do next.


Toolset's code of conduct
=========================

1. If any part of this does not work as advertised, **consider this a bug** and report it.
2. If any part of this gets in your way sometimes, **consider this a bug** and report it.
3. If git is problematic for you, **consider this a bug** (in this set of tools) and report it.
4. If any part of this could work better, consider sharing your ideas for everyone to benefit.
5. Any typo, grammar mistake, misquotation, etc here and anywhere in the tools **shall be considered a bug**,
   so please report it.

Detailed description for the curious
====================================

asdf.

Repo layout
-----------

The repository is structured in the following way::

    .
    ├── README.rst   # symlink to repo_manager/README.rst
    ├── _gitmodules
    ├── .gitmodules
    ├── init_repo.py
    ├── repo_manager
    │   ├── hooks
    │   │   ├── ...
    │   │   └── ...
    │   ├── init
    │   │   ├── ...
    │   │   └── ...
    │   └── README.rst   # this file
    └── < source code >

``.gitmodules``
^^^^^^^^^^^^^^^

Initially this file is empty. This is intentional, to prevent one from using repo without the tools -- it's
too easy to make a small mistake that will make massive problems for others.

Basically, the content of this file is generated.

More precisely, tools try very hard not to record changes to this file to the repository. That would not be a
problem *per se*, but it is a safety net. If the contents would be there, someone somewhere in some time in
the future would call ``git clone --recursive …`` and would forget to get the tools. And the tools try their
best to prevent one from huring himself/herself and others in the team.

``_gitmodules``
^^^^^^^^^^^^^^^

This is the actual list of the submodules. This file is where we record submodules.

``repo_manager``
^^^^^^^^^^^^^^^^

This is a result of ``git subtree add``. Represents the current state of the ``shtack`` tools that shall work
for the current repository.


``init_repo.py``
^^^^^^^^^^^^^^^^

That script is a part of 4-stage bootstrap.

* ``init_repo.py`` is stage-0,

  1) compatible with Py2/3, runs with whatever you system defaults to,
  2) (deprecated) ensures that ``repo_manager`` is present,
  3) runs stage-1 with the same Python it was called by.

* ``repo_manager/init/stage1.py``
  
  1) compatible with Py2/3, runs with whatever you system defaults to,
  2) verifies that the system provides ``git``, any Python 3 version and Python toolset (``pip``,
     ``virtualenv``).
  3) creates virtualenv with Python 3 in ``.git/flowbox/pyenv``,
  4) runs stage-2 with Python provided by freshly created virtualenv.

* ``repo_manager/init/stage2.py``
  
  1) runs with Py3 inside virtualenv,
  2) installs required Python packages,
  3) ensures the installation was successful,
  4) runs stage-3 with the same Python - provided by virtualenv.
     
* ``repo_manager/init/stage3.py``
  
  1) runs with Py3 inside virtualenv,
  2) binds git hooks by symlinking ``.git/hooks`` to ``repo_manager/hooks``,
  3) configures repo,
  4) creates git aliases for the scripts,
  5) fills the ``.gitmodules`` with actual content, initialises and downloads all of the actual submodules,
  6) if ``stack`` tool is not present or is in wrong version, performs its bootstrapping:
     
     a) clones the ``stack`` repo at required revision,
     b) downloads the ``stack`` binary appropriate for your system and uses it to build ``stack`` from repo,
     c) copies the resulting binary to repo root,
     d) displays notice that you shall copy that binary so it is available in ``$PATH``.





FAQ
===

Shtack? What's that?
--------------------

That's what would Sean Connery actually say if he tried to say ``stack``.

You seem overly cautious…
-------------------------

Because I know everyone make mistakes and I want to limit the scope of potential problems. Trust is overrated.

But I don't make mistakes! Don't you believe me?!
-------------------------------------------------

Yes, I don't believe you :D

Honestly though, someone somewhere sometime in the future will be underslept, massively tired and will have
deadline. Any small mistake needs to be caught early and the developer will need assistance.

Or someone will do ``git commit -am '…' && git push`` forgetting to push the contents of the submodules. In
``git`` it's really easy to make mistakes like that.

It's like seatbelts. They're not there because they don't trust you but because accidents *do* happen and
they *will*. They are there to save you in the hard times.

