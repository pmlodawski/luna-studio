#!/bin/sh
find src -name '*.hs' | xargs graphmod -q > dist/doc/images/overview.dot
dot -Tpng -Gdpi=1000 dist/doc/images/overview.dot > dist/doc/images/overview.png
