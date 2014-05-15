#!/bin/bash

echo ">>> Setting up virtualenv in 'python-env' directory"
virtualenv -p $(which python2.7) python-env
cd python-env
. ./bin/activate

echo ">>> Installing rst2html5"
pip install rst2html5

echo ">>> Cloning & installing dev-build of pygments"
hg clone ssh://hg@bitbucket.org/birkenfeld/pygments-main
cd pygments-main
python setup.py install

echo ">>> We're done!"
echo ">>> To start using, run: . ./python-env/bin/activate"
