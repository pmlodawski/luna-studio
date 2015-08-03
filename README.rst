*********************************************************
Shtack - helper scripts for Flowbox FX and New Byte Order
*********************************************************

How to get ``stack``
================

TL;DR
-----

Call this script::

    ./bin/get-stack.sh

Explanation
-----------

Currently, the stable version of stack (``0.1.2.0``) does include **older logic**, i.e. it requires to call
``stack build`` with the precise name. That's not what we want. Fortunatelly, There's new logic that gives
reasonable defaults (vide `github:fpco/stack/issue-493`_).

.. `github:fpco/stack/issue-493`_: https://github.com/commercialhaskell/stack/issues/493#issuecomment-121727358

Therefore, this script basically:

1. downloads ``stack`` binary (with old logic),
2. clones ``stack`` repo on appropriate checkout,
3. if one does not own GHC 7.8, downloads and installs it in ``~/.stack/`` directory,
4. builds ``stack`` with new logic using ``stack`` with old logic,
5. copies the resulting binary to current dir,
6. cleans up temporary files.


Initialize repository; GitLord scripts & hooks
==============================================

TL;DR
-----

Don't touch this repo. Use ``initialize.py`` from your repository.
