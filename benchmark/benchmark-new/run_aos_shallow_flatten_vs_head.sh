#!/bin/bash

# Matches parallel-ml-bench mpl/config/mlton-aos.json at commit b4d8ba41a1a341b3a2390460f9f14e4babbc05ea

# Exit on error
set -e

# Directory of this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TESTS_DIR="${SCRIPT_DIR}/../tests"
OUTPUTS_DIR="${SCRIPT_DIR}/outputs"

# Setting: Extra shared flags passed to both compilers.
# Can be overridden via environment variable (EXTRA_SHARED_FLAGS="..."), CLI (--extra-shared-flags="..."), or by editing here.
EXTRA_SHARED_FLAGS="${EXTRA_SHARED_FLAGS:-}"

# Parse arguments
NAME=""
EXTRA_ARGS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --name=*)
      NAME="${1#*=}"
      shift 1
      ;;
    --name)
      if [[ -n "$2" ]]; then
        NAME="$2"
        shift 2
      else
        echo "Error: --name requires a value" >&2
        exit 1
      fi
      ;;
    --extra_shared_flags=*|--extra-shared-flags=*)
      EXTRA_SHARED_FLAGS="${1#*=}"
      shift 1
      ;;
    --extra_shared_flags|--extra-shared-flags)
      if [[ -n "$2" ]]; then
        EXTRA_SHARED_FLAGS="$2"
        shift 2
      else
        echo "Error: $1 requires a value" >&2
        exit 1
      fi
      ;;
    *)
      EXTRA_ARGS+=("$1")
      shift 1
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

# Compilers + build flags under test
#
# Stripping debug symbols and setting a deterministic magic number are required
# to get identical checksums (otherwise the nondeterministic C filenames and
# compiler-random magic numbers are embedded in the binary)
SHARED_FLAGS='-link-opt -s -build-magic 0 -cc-opt -O2 -cc-opt -march=native'
if [ -n "${EXTRA_SHARED_FLAGS}" ]; then
  SHARED_FLAGS="${SHARED_FLAGS} ${EXTRA_SHARED_FLAGS}"
fi
MLTON0="../../build/bin/mlton"
MLTON0_FLAGS="${SHARED_FLAGS} -disable-pass '(preFlatten.*)|(shallowFlatten.*)'"
MLTON1="../../build/bin/mlton"
MLTON1_FLAGS="${SHARED_FLAGS} -shallow-flatten-max-iters 1 -pre-flatten-max-iters 0 -shallow-flatten-mechanism aos -shallow-flatten-policy maxWidthSameType:4"

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
