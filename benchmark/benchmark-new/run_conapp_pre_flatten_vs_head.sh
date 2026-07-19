#!/bin/bash

# Exit on error
set -e

# Directory of this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TESTS_DIR="${SCRIPT_DIR}/../tests"
OUTPUTS_DIR="${SCRIPT_DIR}/outputs"

# Parse arguments
NAME=""
EXTRA_ARGS=()

for arg in "$@"; do
  case $arg in
    --name=*)
      NAME="${arg#*=}"
      ;;
    *)
      EXTRA_ARGS+=("$arg")
      ;;
  esac
done

if [ -z "$NAME" ]; then
  echo "Error: --name=\$FOO argument is required"
  exit 1
fi

HOSTNAME=$(hostname)
GIT_HASH=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")
DATE=$(date +"%Y%m%d_%H%M%S")
OUTFILE="${OUTPUTS_DIR}/${NAME}:${HOSTNAME}:${GIT_HASH}:${DATE}.jsonl"

# Make sure outputs directory exists
mkdir -p "${OUTPUTS_DIR}"

# Run the benchmark
cd "${TESTS_DIR}"

# Compilers + build flags under test (same as benchmark/run_pre_flatten_vs_head.sh)
#
# Stripping debug symbols and setting a deterministic magic number are required
# to get identical checksums (otherwise the nondeterministic C filenames and
# compiler-random magic numbers are embedded in the binary)
SHARED_FLAGS='-link-opt -s -build-magic 0' 
MLTON0="../../build/bin/mlton"
MLTON0_FLAGS="${SHARED_FLAGS}"
MLTON1="../../build/bin/mlton"
MLTON1_FLAGS="${SHARED_FLAGS} -pre-flatten-max-iters 1 -pre-flatten-consumer-policy always -pre-flatten-resolve-policy local -pre-flatten-types-policy con -pre-flatten-recursive-steps 10 -pre-flatten-phase early -pre-flatten-transfer-policy always"

# Same set of tests as BENCH in benchmark/Makefile
BENCHMARKS=(
  barnes-hut boyer checksum count-graphs DLXSimulator even-odd fft fib flat-array hamlet
  imp-for knuth-bendix lexgen life logic mandelbrot matrix-multiply md5 merge mlyacc
  model-elimination mpuz nucleic output1 peek psdes-random ratio-regions ray raytrace
  simple smith-normal-form string-concat tailfib tak tensor tsp tyan vector32-concat
  vector64-concat vector-rev vliw wc-input1 wc-scanStream zebra zern
)

echo "Running benchmarks and saving output to: ${OUTFILE}"

# Execute benchmark with the specified output file and configurations
../benchmark-new/benchmark \
  -json \
  -outfile "${OUTFILE}" \
  -mlton "${MLTON0} ${MLTON0_FLAGS}" \
  -mlton "${MLTON1} ${MLTON1_FLAGS}" \
  "${EXTRA_ARGS[@]}" \
  "${BENCHMARKS[@]}"
