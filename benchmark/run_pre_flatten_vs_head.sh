#!/bin/bash

# Usage:
#  ./run_mton_vs_mpl.sh [EXTRA_FLAGS]

set -e
set -x

# Wrapper script to compare baseline MLton vs MLton + shallowFlatte

# Compilers + build flags under test
MLTON0="../../build/bin/mlton"
MLTON0_FLAGS=
MLTON1="../../build/bin/mlton"
# MLTON1_FLAGS='-pre-flatten-max-iters 1 -pre-flatten-consumer-policy all_unpack -pre-flatten-resolve-policy global -pre-flatten-types-policy tuple -pre-flatten-post-steps shrink,flatten,shrink -pre-flatten-recursive-steps 10'
MLTON1_FLAGS='-pre-flatten-max-iters 1 -pre-flatten-consumer-policy always -pre-flatten-resolve-policy local -pre-flatten-types-policy tuple -pre-flatten-recursive-steps 10 -pre-flatten-phase late -pre-flatten-transfer-policy tail_only'
BFLAGS="-mlton '$MLTON0 $MLTON0_FLAGS' -mlton '$MLTON1 $MLTON1_FLAGS'"

EXTRA_FLAGS="$@"
make test BFLAGS="$BFLAGS $EXTRA_FLAGS"
