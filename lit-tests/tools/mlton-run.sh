#!/bin/bash
#
# Wrapper script to compile a binary under `mlton` and print the output
set -e
SCRIPT_DIR=$(dirname $(realpath $0))
MLTON=${SCRIPT_DIR}/../../build/bin/mlton
OUTDIR=$(mktemp -d)

cleanup() {
    rm -rf "$OUTDIR"
}
trap cleanup EXIT

OUTFILE=${OUTDIR}/out.bin
${MLTON} -output ${OUTFILE} "$@"
${OUTFILE}
