#!/bin/bash
set -x
# Setting: Extra shared flags passed to both compilers.
# Can be overridden via environment variable (EXTRA_SHARED_FLAGS="..."), CLI (--extra-shared-flags="..."), or by editing here.
EXTRA_SHARED_FLAGS="${EXTRA_SHARED_FLAGS:-}"

EXTRA_ARGS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
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

cd "$(dirname "$0")/../tests" || exit 1

SHARED_FLAGS=""
if [ -n "${EXTRA_SHARED_FLAGS}" ]; then
  SHARED_FLAGS="${EXTRA_SHARED_FLAGS} "
fi

../benchmark-new/benchmark -max-bench-count 1 \
  -mlton "../../build/bin/mlton ${SHARED_FLAGS}-disable-pass '(preFlatten.*)|(shallowFlatten.*)'" \
  -mlton "../../build/bin/mlton ${SHARED_FLAGS}-pre-flatten-max-iters 1 -pre-flatten-consumer-policy always -pre-flatten-resolve-policy local -pre-flatten-types-policy tuple -pre-flatten-recursive-steps 10 -pre-flatten-phase late -pre-flatten-transfer-policy tail_only" \
  "${EXTRA_ARGS[@]}" empty

