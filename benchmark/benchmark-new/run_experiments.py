#!/usr/bin/env python3

import argparse
from datetime import datetime
import os
from pathlib import Path
import re
import shlex
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

BUILD_TYPE_FLAGS = {
    "default": "",
    "dump_ir": "-keep-pass-out-dir bin -keep-pass '.*'",
    "diagnostic": "-keep-pass-out-dir bin -diag-pass '.*' -keep-pass '.*' -verbose 3",
    "profile": "-keep-pass-out-dir bin -diag-pass '.*' -keep-pass '.*' -verbose 3 -keep g -cc-opt '-g2' -link-opt '-lprofiler'",
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
    config_choices = list(CONFIG_FLAGS.keys()) + ["none"]

    parser.add_argument(
        "--test_config",
        "--test-config",
        "--test",
        "-t",
        dest="test_config",
        required=not any(arg in sys.argv for arg in ["--list-benchmarks", "--list-configs", "--list-build-types", "-h", "--help"]),
        choices=config_choices,
        help=f"Test compiler configuration (required, choices: {', '.join(config_choices)})",
    )
    parser.add_argument(
        "--base_config",
        "--base-config",
        "--base",
        dest="base_config",
        default="baseline",
        choices=config_choices,
        help=f"Base compiler configuration (default: baseline, choices: {', '.join(config_choices)})",
    )
    parser.add_argument(
        "--build_type",
        "--build-type",
        dest="build_type",
        default="default",
        choices=list(BUILD_TYPE_FLAGS.keys()),
        help=f"Build type mode (default: default, choices: {', '.join(BUILD_TYPE_FLAGS.keys())})",
    )
    parser.add_argument(
        "--name",
        dest="name",
        required=not any(arg in sys.argv for arg in ["--list-benchmarks", "--list-configs", "--list-build-types", "-h", "--help"]),
        help="Run identifier used to populate the output file name",
    )
    parser.add_argument(
        "--mlton",
        default="../../build/bin/mlton",
        help="Path to MLton executable (default: '../../build/bin/mlton')",
    )
    parser.add_argument(
        "--capture_profile",
        "--capture-profile",
        dest="capture_profile",
        action="store_true",
        default=False,
        help="Capture CPU profile during benchmark compilation",
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
    parser.add_argument(
        "--list-build-types",
        action="store_true",
        help="List all available build types and their flags, then exit",
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
        print("  none:")
        print("    flags: (skip configuration side)")
        sys.exit(0)

    if args.list_build_types:
        print("Available build types:")
        for name, flags in BUILD_TYPE_FLAGS.items():
            print(f"  {name}:")
            print(f"    flags: {flags or '(none)'}")
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
    bin_dir = tests_dir / "bin"
    bin_dir.mkdir(parents=True, exist_ok=True)

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

    if args.base_config == "none" and args.test_config == "none":
        print(
            "Error: At least one of base_config or test_config must not be 'none'.",
            file=sys.stderr,
        )
        sys.exit(1)

    build_type_flags = BUILD_TYPE_FLAGS[args.build_type]

    def build_mlton_args(bench: str | None = None) -> list[str]:
        mlton_args = []
        if args.base_config != "none":
            base_flags = CONFIG_FLAGS[args.base_config]
            if build_type_flags:
                base_flags = f"{base_flags} {build_type_flags}"
            prefix = ""
            if args.capture_profile and bench:
                prefix = f"CPUPROFILE=bin/{bench}_{args.base_config}.prof LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libprofiler.so "
            mlton_args.extend(["-mlton", f"{prefix}{args.mlton} {base_flags}"])

        if args.test_config != "none":
            test_flags = CONFIG_FLAGS[args.test_config]
            if build_type_flags:
                test_flags = f"{test_flags} {build_type_flags}"
            prefix = ""
            if args.capture_profile and bench:
                prefix = f"CPUPROFILE=bin/{bench}_{args.test_config}.prof LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libprofiler.so "
            mlton_args.extend(["-mlton", f"{prefix}{args.mlton} {test_flags}"])
        return mlton_args

    if not args.capture_profile:
        mlton_args = build_mlton_args()
        cmd = [
            str(benchmark_bin),
            "-json",
            "-outfile",
            str(outfile),
            *mlton_args,
            *extra_args,
            *selected_benchmarks,
        ]

        if args.dry_run:
            full_cmd = f"(cd {shlex.quote(str(tests_dir))} && {shlex.join(cmd)})"
            print(full_cmd)
            sys.exit(0)

        print(f"Running benchmarks and saving output to: {outfile}")
        sys.stdout.flush()

        try:
            proc = subprocess.run(cmd, cwd=tests_dir)
            sys.exit(proc.returncode)
        except KeyboardInterrupt:
            print("\nBenchmark run interrupted by user.", file=sys.stderr)
            sys.exit(130)
    else:
        if args.dry_run:
            for b in selected_benchmarks:
                mlton_args = build_mlton_args(b)
                cmd = [
                    str(benchmark_bin),
                    "-json",
                    "-outfile",
                    str(outfile),
                    *mlton_args,
                    *extra_args,
                    b,
                ]
                full_cmd = f"(cd {shlex.quote(str(tests_dir))} && {shlex.join(cmd)})"
                print(full_cmd)
            sys.exit(0)

        print(f"Running benchmarks and saving output to: {outfile}")
        sys.stdout.flush()

        if len(selected_benchmarks) == 1:
            b = selected_benchmarks[0]
            mlton_args = build_mlton_args(b)
            cmd = [
                str(benchmark_bin),
                "-json",
                "-outfile",
                str(outfile),
                *mlton_args,
                *extra_args,
                b,
            ]
            try:
                proc = subprocess.run(cmd, cwd=tests_dir)
                sys.exit(proc.returncode)
            except KeyboardInterrupt:
                print("\nBenchmark run interrupted by user.", file=sys.stderr)
                sys.exit(130)
        else:
            outfile.write_text("")
            for b in selected_benchmarks:
                mlton_args = build_mlton_args(b)
                temp_out = outputs_dir / f".tmp_{args.name}_{b}_{date}.jsonl"
                cmd = [
                    str(benchmark_bin),
                    "-json",
                    "-outfile",
                    str(temp_out),
                    *mlton_args,
                    *extra_args,
                    b,
                ]
                try:
                    proc = subprocess.run(cmd, cwd=tests_dir)
                    if temp_out.exists():
                        with open(outfile, "a") as f_out, open(temp_out, "r") as f_in:
                            f_out.write(f_in.read())
                        temp_out.unlink()
                    if proc.returncode != 0:
                        sys.exit(proc.returncode)
                except KeyboardInterrupt:
                    if temp_out.exists():
                        temp_out.unlink()
                    print("\nBenchmark run interrupted by user.", file=sys.stderr)
                    sys.exit(130)
            sys.exit(0)


if __name__ == "__main__":
    main()
