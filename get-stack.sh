#!/usr/bin/env bash

yell() { echo "$0: $*" >&2; }
die() { RETCODE=$1; shift; yell "$*"; exit ${RETCODE}; }
try() { "$@" || die 111 "cannot $*"; }


goDarwin() {

    GHCVER="$(ghc --version)"
    [[ ${GHCVER} =~ 7.8.* ]] || die 3 "sorry, wrong GHC version. Run with 7.8.x. If you don't have: see the comment in the script"
    # in case of above error, just enable line labaled  ########## GET_THE_DAMN_GHC_7.8 ##########  (you'll notice)


    TMPDIR=$(mktemp -d -t stacker_XXXX)
    CURDIR=$(pwd)

    echo "${TMPDIR}"
    pushd "${TMPDIR}"

    curl -LvO 'https://github.com/commercialhaskell/stack/releases/download/v0.1.2.0/stack-0.1.2.0-x86_64-osx.gz'
    # curl -LvO 'https://github.com/commercialhaskell/stack/releases/download/v0.1.2.0/stack-0.1.2.0-x86_64-osx.gz.asc'
    gunzip stack-0.1.2.0-x86_64-osx.gz
    chmod +x stack-0.1.2.0-x86_64-osx

    git clone git@github.com:commercialhaskell/stack.git
    cd stack
    git checkout c5b98565e0401453ba3c86c01a41db64dff5b69c
    # ../stack-0.1.2.0-x86_64-osx setup                        ########## GET_THE_DAMN_GHC_7.8 ##########
    ../stack-0.1.2.0-x86_64-osx build -j 8

    cp ".stack-work/install/x86_64-osx/lts-2.17/7.8.4/bin/stack" "${CURDIR}/stack"
    popd
    rm -rf "${TMPDIR}"

    echo "##############################################################################"
    echo "#                                                                            #"
    echo "# My job is done, you have freshly built './stack'. Put that in e.g. '~/bin' #"
    echo "#                                                                            #"
    echo "##############################################################################"
}

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    yell "NOT IMPLEMENTED FOR ${OSTYPE}"
    exit 1
elif [[ "$OSTYPE" == "darwin"* ]]; then
    goDarwin
elif [[ "$OSTYPE" == "cygwin" ]]; then
    yell "NOT IMPLEMENTED FOR ${OSTYPE}"
    exit 1
elif [[ "$OSTYPE" == "msys" ]]; then
    yell "NOT IMPLEMENTED FOR ${OSTYPE}"
    exit 1
elif [[ "$OSTYPE" == "win32" ]]; then
    yell "NOT IMPLEMENTED FOR ${OSTYPE}"
    exit 1
elif [[ "$OSTYPE" == "freebsd"* ]]; then
    yell "NOT IMPLEMENTED FOR ${OSTYPE}"
    exit 1
else
    yell "OSTYPE='${OSTYPE}' NOT RECOGNIZED"
    exit 2
fi
