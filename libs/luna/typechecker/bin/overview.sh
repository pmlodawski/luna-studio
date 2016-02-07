#!/bin/sh
find src -name '*.hs' | xargs graphmod -q | dot -Tpng -Gdpi=1000 > dist/doc/images/overview.png
