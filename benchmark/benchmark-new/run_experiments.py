#!/usr/bin/env python3

import argparse
from datetime import datetime
import os
from pathlib import Path
import re
import socket
import subprocess
import sys

BENCHMARKS = [
    "barnes-hut", "boyer", "checksum", "count-graphs", "DLXSimulator",
    "even-odd", "fft", "fib", "flat-array", "hamlet",
    "imp-for", "knuth-bendix", "lexgen", "life", "logic",
    "mandelbrot", "matrix-multiply", "md5", "merge", "mlyacc",
    "model-elimination", "mpuz", "nucleic", "output1", "peek",
    "psdes-random", "ratio-regions", "ray", "raytrace", "simple",
    "smith-normal-form", "string-concat", "tailfib", "tak", "tensor",
    "tsp", "tyan", "vector32-concat", "vector64-concat", "vector-rev",
    "vliw", "wc-input1", "wc-scanStream", "zebra", "zern",
]

SHARED_FLAGS = "-link-opt -s -build-magic 0"

CONFIG_FLAGS = {
    "baseline": f"{SHARED_FLAGS} -disable-pass '(preFlatten.*)|(shallowFlatten.*)'",
    "aos": f"{SHARED_FLAGS} -shallow-flatten-max-iters 1 -pre-flatten-max-iters 0 -shallow-flatten-mechanism aos -shallow-flatten-policy maxWidthSameType:4",
    "soa": f"{SHARED_FLAGS} -shallow-flatten-max-iters 1 -pre-flatten-max-iters 0 -shallow-flatten-mechanism soa -shallow-flatten-policy maxWidthSameType:4",
    "conapp": f"{SHARED_FLAGS} -pre-flatten-max-iters 1 -pre-flatten-consumer-policy always -pre-flatten-resolve-policy local -pre-flatten-types-policy con -pre-flatten-recursive-steps 10 -pre-flatten-phase early -pre-flatten-transfer-policy always",
    "tuple": f"{SHARED_FLAGS} -pre-flatten-max-iters 1 -pre-flatten-consumer-policy always -pre-flatten-resolve-policy local -pre-flatten-types-policy tuple -pre-flatten-recursive-steps 10 -pre-flatten-phase late -pre-flatten-transfer-policy tail_only",
}


def get_hostname() -> str:
    try:
        res = subprocess.run(
            ["hostname"],
            capture_output=True,
            text=True,
            check=True,
        )
        out = res.stdout.strip()
        if out:
            return out
    except Exception:
        pass
    return socket.gethostname() or "unknown"


def get_git_hash(repo_dir: Path) -> str:
    try:
        res = subprocess.run(
            ["git", "rev-parse", "--short", "HEAD"],
            cwd=repo_dir,
            capture_output=True,
            text=True,
            check=True,
        )
        out = res.stdout.strip()
        if out:
            return out
    except Exception:
        pass
    return "unknown"


def main():
    parser = argparse.ArgumentParser(
        description="Run MLton benchmark experiments comparing different compiler configurations."
    )
    parser.add_argument(
        "--benchmark",
        "--benchmarks",
        "-b",
        dest="benchmark",
        default=".*",
        help="Regex pattern to filter benchmarks (default: '.*')",
    )
    parser.add_argument(
        "--test_config",
        "--test-config",
        "--test",
        "-t",
        dest="test_config",
        required=not any(arg in sys.argv for arg in ["--list-benchmarks", "--list-configs", "-h", "--help"]),
        choices=list(CONFIG_FLAGS.keys()),
        help=f"Test compiler configuration (required, choices: {', '.join(CONFIG_FLAGS.keys())})",
    )
    parser.add_argument(
        "--base_config",
        "--base-config",
        "--base",
        dest="base_config",
        default="baseline",
        choices=list(CONFIG_FLAGS.keys()),
        help=f"Base compiler configuration (default: baseline, choices: {', '.join(CONFIG_FLAGS.keys())})",
    )
    parser.add_argument(
        "--name",
        dest="name",
        required=not any(arg in sys.argv for arg in ["--list-benchmarks", "--list-configs", "-h", "--help"]),
        help="Run identifier used to populate the output file name",
    )
    parser.add_argument(
        "--mlton",
        default="../../build/bin/mlton",
        help="Path to MLton executable (default: '../../build/bin/mlton')",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Print the benchmark command without executing it",
    )
    parser.add_argument(
        "--list-benchmarks",
        action="store_true",
        help="List all available benchmarks and exit",
    )
    parser.add_argument(
        "--list-configs",
        action="store_true",
        help="List all available configurations and their flags, then exit",
    )

    args, extra_args = parser.parse_known_args()

    if args.list_benchmarks:
        print("Available benchmarks (45 total):")
        for b in BENCHMARKS:
            print(f"  {b}")
        sys.exit(0)

    if args.list_configs:
        print("Available configurations:")
        for name, flags in CONFIG_FLAGS.items():
            print(f"  {name}:")
            print(f"    flags: {flags}")
        sys.exit(0)

    # Compile benchmark regex
    try:
        bench_re = re.compile(args.benchmark)
    except re.error as e:
        print(f"Error: Invalid benchmark regex '{args.benchmark}': {e}", file=sys.stderr)
        sys.exit(1)

    # Filter benchmarks
    selected_benchmarks = [b for b in BENCHMARKS if bench_re.search(b)]
    if not selected_benchmarks:
        print(
            f"Error: No benchmarks matched regex '{args.benchmark}'. Available benchmarks:\n"
            + ", ".join(BENCHMARKS),
            file=sys.stderr,
        )
        sys.exit(1)

    script_dir = Path(__file__).resolve().parent
    tests_dir = (script_dir / "../tests").resolve()
    outputs_dir = script_dir / "outputs"
    outputs_dir.mkdir(parents=True, exist_ok=True)

    benchmark_bin = script_dir / "benchmark"
    if not benchmark_bin.is_file():
        print(
            f"Error: Benchmark executable not found at {benchmark_bin}. Please run `make` in {script_dir} first.",
            file=sys.stderr,
        )
        sys.exit(1)

    hostname = get_hostname()
    git_hash = get_git_hash(script_dir)
    date = datetime.now().strftime("%Y%m%d_%H%M%S")
    outfile = outputs_dir / f"{args.name}:{hostname}:{git_hash}:{date}.jsonl"

    base_flags = CONFIG_FLAGS[args.base_config]
    test_flags = CONFIG_FLAGS[args.test_config]

    cmd = [
        str(benchmark_bin),
        "-json",
        "-outfile",
        str(outfile),
        "-mlton",
        f"{args.mlton} {base_flags}",
        "-mlton",
        f"{args.mlton} {test_flags}",
        *extra_args,
        *selected_benchmarks,
    ]

    print(f"Running benchmarks and saving output to: {outfile}")
    sys.stdout.flush()

    if args.dry_run:
        print("\n[Dry Run] Working directory:", tests_dir)
        print("[Dry Run] Command:")
        print(" ".join(f"'{c}'" if " " in c else c for c in cmd))
        sys.exit(0)

    try:
        proc = subprocess.run(cmd, cwd=tests_dir)
        sys.exit(proc.returncode)
    except KeyboardInterrupt:
        print("\nBenchmark run interrupted by user.", file=sys.stderr)
        sys.exit(130)


if __name__ == "__main__":
    main()
