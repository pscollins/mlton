#!/bin/bash
#
# Wrapper script to compile a binary under `mlton` and print the generated C
#
# Requires that it is called as:
#   mlton-print-c.sh [OPTIONAL FLAGS] source_file.sml
set -e
SCRIPT_DIR=$(dirname $(realpath $0))
MLTON=${SCRIPT_DIR}/../../build/bin/mlton

# Check if OVERRIDE_OUTDIR is provided and non-empty
if [ -n "$OVERRIDE_OUTDIR" ]; then
    OUTDIR="$OVERRIDE_OUTDIR"
    mkdir -p "$OUTDIR"
else
    OUTDIR=$(mktemp -d)
    cleanup() {
        rm -rf "$OUTDIR"
    }
    trap cleanup EXIT
fi

OUTFILE=${OUTDIR}/out.bin
mkdir -p ${OUTDIR}

# Extract the last argument and make it absolute
LAST_ARG=$(realpath "${@: -1}")
OTHER_ARGS=("${@:1:$#-1}")

# Pass the earlier arguments as-is, use the absolute path for the last one, and compile using the C codegen with kept intermediate files
${MLTON} -codegen c -output ${OUTFILE} -keep g "${OTHER_ARGS[@]}" "$LAST_ARG"

# Print the generated C code
cat ${OUTFILE}.*.c
