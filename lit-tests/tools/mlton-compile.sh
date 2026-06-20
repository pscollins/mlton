#!/bin/bash
#
# Wrapper script to compile a binary under `mlton` and keep any output files in
# the LIT test-supplied %t.
#
# Requires that it is called in the LIT test as
#
#   mlton-compile.sh [OPTIONAL FLAGS] source_file.sml %t
set -e
SCRIPT_DIR=$(dirname $(realpath $0))
MLTON=${SCRIPT_DIR}/../../build/bin/mlton
# Check if OVERRIDE_OUTDIR is provided and non-empty
if [ -n "$OVERRIDE_OUTDIR" ]; then
    OUTDIR="$OVERRIDE_OUTDIR"
    # If the last argument is the input file (ends in .sml, .mlb, etc.),
    # then there is no output directory argument.
    LAST_ARG="${@: -1}"
    if [[ "$LAST_ARG" =~ \.(sml|mlb|fun|sig)(\.disabled)?$ ]]; then
        INFILE=$(realpath "$LAST_ARG")
        COMPILE_ARGS=("${@: 1: $# - 1}")
    else
        INFILE=$(realpath "${@: -2: 1}")
        COMPILE_ARGS=("${@: 1: $# - 2}")
    fi
else
    # Out directory must be the last argument
    OUTDIR="${@: -1}"
    # Input file must be the second to last argument, and we make it absolute here
    INFILE=$(realpath "${@: -2: 1}")
    # Everything beforehand gets passed to the compiler
    COMPILE_ARGS=("${@: 1: $# - 2}")
fi

OUTFILE=${OUTDIR}/out.bin
mkdir -p ${OUTDIR}
${MLTON} -inline 0 -output ${OUTFILE} -keep-pass-out-dir ${OUTDIR} "${COMPILE_ARGS[@]}" "$INFILE"
