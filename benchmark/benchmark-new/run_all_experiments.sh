#!/bin/bash

# Exit on error
set -e

# Directory of this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Parse arguments
BASE=""
FLAVORS="aos,soa,conapp,tuple"
EXTRA_ARGS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --base_nick)
      if [[ -n "$2" ]]; then
        BASE="$2"
        shift 2
      else
        echo "Error: --base_nick requires a value" >&2
        exit 1
      fi
      ;;
    --base_nick=*)
      BASE="${1#*=}"
      shift 1
      ;;
    --flavors)
      if [[ -n "$2" ]]; then
        FLAVORS="$2"
        shift 2
      else
        echo "Error: --flavors requires a value" >&2
        exit 1
      fi
      ;;
    --flavors=*)
      FLAVORS="${1#*=}"
      shift 1
      ;;
    *)
      EXTRA_ARGS+=("$1")
      shift 1
      ;;
  esac
done

if [ -z "$BASE" ]; then
  echo "Error: --base_nick \$BASE argument is required" >&2
  exit 1
fi

# Run the experiments for the specified flavors
IFS=',' read -ra FLAVOR_ARRAY <<< "$FLAVORS"
for flavor in "${FLAVOR_ARRAY[@]}"; do
  flavor="${flavor//[[:space:]]/}"
  case "$flavor" in
    aos)
      "${SCRIPT_DIR}/run_aos_shallow_flatten_vs_head.sh" --name="test_aos_flatten_${BASE}" "${EXTRA_ARGS[@]}"
      ;;
    soa)
      "${SCRIPT_DIR}/run_soa_shallow_flatten_vs_head.sh" --name="test_soa_flatten_${BASE}" "${EXTRA_ARGS[@]}"
      ;;
    con|conapp)
      "${SCRIPT_DIR}/run_conapp_pre_flatten_vs_head.sh" --name="test_conapp_flatten_${BASE}" "${EXTRA_ARGS[@]}"
      ;;
    tuple)
      "${SCRIPT_DIR}/run_tuple_pre_flatten_vs_head.sh" --name="test_tuple_flatten_${BASE}" "${EXTRA_ARGS[@]}"
      ;;
    *)
      echo "Error: Unknown flavor '$flavor'" >&2
      exit 1
      ;;
  esac
done
