# MLton Benchmark Runner (New)

This directory contains the benchmark runner for MLton, which supports JSON/JSONL output formats, configurable iteration limits, and comparing different compiler configurations.

## Compilation

To compile the benchmark runner:

```bash
make
```

---

## Running Experiments with `run_experiments.py`

[`run_experiments.py`](run_experiments.py) provides a unified CLI to run benchmarks across compiler configurations with regex-based benchmark filtering and customizable base/test configurations.

### Basic Syntax

```bash
./run_experiments.py --benchmark=$BM_RE --test_config=$TEST --base_config=$BASE --name=$NAME
```

### Options

| Option | Shorthand / Aliases | Description | Default |
|---|---|---|---|
| `--test_config` | `--test`, `--test-config`, `-t` | **(Required)** Target compiler configuration | — |
| `--name` | | **(Required)** Run name identifier for the output file | — |
| `--base_config` | `--base`, `--base-config` | Baseline compiler configuration | `baseline` |
| `--benchmark` | `--benchmarks`, `-b` | Regex pattern to filter benchmarks | `.*` |
| `--mlton` | | Path to the MLton compiler executable | `../../build/bin/mlton` |
| `--dry-run` | | Print the command that would be executed without running it | `False` |
| `--list-benchmarks` | | List all 45 available benchmarks and exit | `False` |
| `--list-configs` | | List all supported configurations and their flags | `False` |

*Any additional arguments (such as `-max-bench-count` or `-once`) are automatically forwarded to the underlying `benchmark` binary.*

### Supported Compiler Configurations

| Configuration | Description | Flags |
|---|---|---|
| **`baseline`** | Baseline compiler (disables pre-flatten and shallow-flatten passes) | `-link-opt -s -build-magic 0 -cc-opt -O2 -cc-opt -march=native -disable-pass '(preFlatten.*)\|(shallowFlatten.*)'` |
| **`aos`** | Array-of-Structures shallow flattening | `-link-opt -s -build-magic 0 -cc-opt -O2 -cc-opt -march=native -shallow-flatten-max-iters 1 -pre-flatten-max-iters 0 -shallow-flatten-mechanism aos -shallow-flatten-policy maxWidthSameType:4` |
| **`soa`** | Structure-of-Arrays shallow flattening | `-link-opt -s -build-magic 0 -cc-opt -O2 -cc-opt -march=native -shallow-flatten-max-iters 1 -pre-flatten-max-iters 0 -shallow-flatten-mechanism soa -shallow-flatten-policy maxWidthSameType:4` |
| **`conapp`** | Constructor application pre-flattening | `-link-opt -s -build-magic 0 -cc-opt -O2 -cc-opt -march=native -pre-flatten-max-iters 1 -pre-flatten-consumer-policy always -pre-flatten-resolve-policy local -pre-flatten-types-policy con -pre-flatten-recursive-steps 10 -pre-flatten-phase early -pre-flatten-transfer-policy always` |
| **`tuple`** | Tuple pre-flattening | `-link-opt -s -build-magic 0 -cc-opt -O2 -cc-opt -march=native -pre-flatten-max-iters 1 -pre-flatten-consumer-policy always -pre-flatten-resolve-policy local -pre-flatten-types-policy tuple -pre-flatten-recursive-steps 10 -pre-flatten-phase late -pre-flatten-transfer-policy tail_only` |
| **`none`** | Skip this configuration side (e.g. to run only the test or base config) | *(none)* |

### Output File Format

Benchmark results are written in JSONL format to `outputs/`:

```
outputs/${NAME}:${HOSTNAME}:${GIT_HASH}:${DATE}.jsonl
```

Example filename:
```
outputs/test_tuple_run:myhost:09fe6138b:20260822_150512.jsonl
```

### Usage Examples

#### 1. Full Benchmark Run
Run the full benchmark suite comparing `aos` against `baseline`:
```bash
./run_experiments.py --test_config=aos --name=full_aos_run
```

Run comparing `tuple` against `baseline`:
```bash
./run_experiments.py --test_config=tuple --name=full_tuple_run
```

#### 2. Custom Base Configuration
Compare `soa` directly against `aos` instead of `baseline`:
```bash
./run_experiments.py --test_config=soa --base_config=aos --name=soa_vs_aos
```

#### 3. Filtering Benchmarks with Regex

- **Exact match single benchmark (`fib`):**
  ```bash
  ./run_experiments.py --benchmark='^fib$' --test_config=tuple --name=fib_test
  ```

- **Multiple benchmarks matching pattern (`fib`, `tailfib`, `tak`, `fft`):**
  ```bash
  ./run_experiments.py --benchmark='(fib|tak|fft)' --test_config=conapp --name=subset_test
  ```

- **Prefix matching all vector benchmarks (`vector32-concat`, `vector64-concat`, `vector-rev`):**
  ```bash
  ./run_experiments.py --benchmark='^vector' --test_config=soa --name=vector_benchmarks
  ```

#### 4. Fast Smoke / Verification Test
To quickly verify that benchmarks compile and run without waiting for full iterations, pass `-max-bench-count 1 -once`:
```bash
./run_experiments.py --benchmark='^fib$' --test_config=aos --name=quick_test -max-bench-count 1 -once
```

#### 5. Dry Run
Inspect the exact command that would be run:
```bash
./run_experiments.py --benchmark='^fib$' --test_config=conapp --name=dry_test --dry-run
```

#### 6. Listing Available Benchmarks and Configs
```bash
./run_experiments.py --list-benchmarks
./run_experiments.py --list-configs
```

---

## Running Shell Script Benchmarks

In addition to `run_experiments.py`, dedicated shell scripts are provided for specific configurations.

### Usage

Each script requires a `--name` argument to identify the run. Results are saved as JSONL files under `outputs/`.

#### 1. Full Benchmark Run
To run the full suite with default benchmark counts:

```bash
./run_aos_shallow_flatten_vs_head.sh --name=my_aos_benchmark_run
./run_soa_shallow_flatten_vs_head.sh --name=my_soa_benchmark_run
./run_conapp_pre_flatten_vs_head.sh --name=my_conapp_benchmark_run
./run_tuple_pre_flatten_vs_head.sh --name=my_tuple_benchmark_run
```

This will run all benchmarks and output the JSONL file to `outputs/${NAME}:${HOSTNAME}:${GIT_HASH}:${DATE}.jsonl`.

#### 2. Quick Verification Run (Recommended for testing)
To verify that everything compiles and runs correctly without waiting for the full iteration counts (which can take up to an hour), you can restrict each benchmark to a single iteration and a single trial run:

```bash
./run_tuple_pre_flatten_vs_head.sh --name=init_test -max-bench-count 1 -once
```

### Passing Extra Arguments

You can pass extra arguments to the benchmark binary by appending them to the script invocation. For example, to run only a subset of benchmarks or specify other runner-specific options:

- To run only the `fib` and `fft` benchmarks:
  ```bash
  ./run_tuple_pre_flatten_vs_head.sh --name=subset_test -max-bench-count 1 -once fib fft
  ```
- To run with a custom benchmark count limit:
  ```bash
  ./run_tuple_pre_flatten_vs_head.sh --name=limit_test -max-bench-count 10
  ```

### Batch Scripts

- **`run_all_experiments.sh`**: Runs all 4 configurations (`aos`, `soa`, `conapp`, `tuple`) in sequence:
  ```bash
  ./run_all_experiments.sh --base_nick=my_run_label
  ```
- **`run_shallow_flatten_experiments.sh`**: Runs shallow flattening configurations (`aos`, `soa`) in sequence:
  ```bash
  ./run_shallow_flatten_experiments.sh --base_nick=my_run_label
  ```

---

## Viewing Results with `print_speedup.py`

[`print_speedup.py`](print_speedup.py) parses benchmark JSONL output files and displays formatted ASCII tables comparing compiler configurations and computing relative speedups.

### Basic Usage

```bash
# View speedup table for the most recent benchmark output
./print_speedup.py

# View a specific output file by name, path, or prefix
./print_speedup.py outputs/test_tuple_run:myhost:09fe6138b:20260822_150512.jsonl
./print_speedup.py test_conapp_flatten_mlton_mlton_o3
```

### Options

| Option | Shorthand / Aliases | Description | Default |
|---|---|---|---|
| `[file]` | `-i`, `--input`, `-f`, `--file` | JSONL output file path or name | Most recent in `outputs/` |
| `--sort` | `-s` | Sort rows (`default`, `name`, `speedup`, `speedup-asc`, `diff`, `base`, `test`) | `default` (suite order) |
| `--benchmark` | `--filter`, `-b` | Regex pattern to filter benchmark names | `.*` |
| `--metric` | `-m` | Metric to compare (`runtime`, `compile`, `size`, `all`) | `runtime` |
| `--format` | | Output format (`ascii`, `markdown`, `plain`, `csv`, `tsv`) | `ascii` |
| `--base` | | Baseline compiler abbreviation | `MLton0` |
| `--test` | | Test compiler abbreviation | `MLton1` |
| `--quiet` | `-q` | Suppress metadata header and print only the table | `False` |
| `--list` | `-l` | List all available benchmark output files with timestamps | `False` |
| `--precision` | `-p` | Decimal precision for timing values | `4` |

### Examples

```bash
# Sort by highest speedup first
./print_speedup.py --sort speedup

# Filter benchmarks matching regex
./print_speedup.py --benchmark '(fib|tak|vector|wc)'

# Compare compile time or binary size
./print_speedup.py --metric compile
./print_speedup.py --metric size
./print_speedup.py --metric all

# Export as Markdown or CSV
./print_speedup.py --format markdown
./print_speedup.py --format csv

# List all available benchmark runs
./print_speedup.py --list
```

