#!/bin/bash

# Usage:
#  ./run_mton_vs_mpl.sh [EXTRA_FLAGS]

set -e
set -x

# Wrapper script to compare MPL (without `preFlatten`) vs MLTon

# Compilers + build flags under test
MLTON0="../../build/bin/mlton"
MLTON0_FLAGS=
MLTON1='~/code/mpl/build/bin/mpl'
MLTON1_FLAGS='-pre-flatten-max-iters 0'
BFLAGS="-mlton '$MLTON0 $MLTON0_FLAGS' -mlton '$MLTON1 $MLTON1_FLAGS'"

EXTRA_FLAGS="$@"
make test BFLAGS="$BFLAGS $EXTRA_FLAGS"
