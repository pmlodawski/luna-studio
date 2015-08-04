#!/usr/bin/env bash

yell() { echo "$0: $*" >&2; }
die() { RETCODE=$1; shift; yell "$*"; exit ${RETCODE}; }
try() { "$@" || die 111 "cannot $*"; }


STACK_HEAD="c5b98565e0401453ba3c86c01a41db64dff5b69c"



goUnixDeriv() {
    # This works for both Darwin and Linux
    # Arguments:
    ALLOW_STACK_SETUP=$1
    STACK_VERSION=$2
    STACK_ARCH=$3
    STACK_OS_ABBREV=$4
    BUILD_JOBS=$5

    TMPDIR=$(mktemp -d -t /tmp/stacker_XXXX)
    [ $? -eq 0 ] || die 4 "Sorry, you do not have the \`mktemp\` command"
    trap "echo 'There are leftovers in TMPDIR=${TMPDIR} - diagnose and/or remove at will'" EXIT

    GHCVER="$(ghc --version)"
    CURDIR=$(pwd)

    pushd "${TMPDIR}"

    curl -LO "https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/stack-${STACK_VERSION}-${STACK_ARCH}-${STACK_OS_ABBREV}.gz"
    gunzip "stack-${STACK_VERSION}-${STACK_ARCH}-${STACK_OS_ABBREV}.gz"
    chmod +x "stack-${STACK_VERSION}-${STACK_ARCH}-${STACK_OS_ABBREV}"

    git clone git@github.com:commercialhaskell/stack.git
    cd stack
    git checkout "${STACK_HEAD}"
    
    ${ALLOW_STACK_SETUP} && "../stack-${STACK_VERSION}-${STACK_ARCH}-${STACK_OS_ABBREV}" setup || exit 6

    "../stack-${STACK_VERSION}-${STACK_ARCH}-${STACK_OS_ABBREV}" build -j "${BUILD_JOBS}" || exit 7

    cp ".stack-work/install/x86_64-${STACK_OS_ABBREV}/lts-2.17/*/bin/stack" "${CURDIR}/stack"

    rm -rf "${TMPDIR}"
    trap - EXIT

    echo "##############################################################################"
    echo "#                                                                            #"
    echo "# My job is done, you have freshly built './stack'. Put that in e.g. '~/bin' #"
    echo "#                                                                            #"
    echo "##############################################################################"
}


if [[ "$OSTYPE" == "linux"* ]]; then
    goUnixDeriv true "0.1.2.0" "x86_64" "linux" "8"

elif [[ "$OSTYPE" == "darwin"* ]]; then
    goUnixDeriv true "0.1.2.0" "x86_64" "osx" "8"

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
