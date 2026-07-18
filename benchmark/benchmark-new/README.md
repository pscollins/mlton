# MLton Benchmark Runner (New)

This directory contains the new benchmark runner for MLton, which supports JSON/JSONL output formats, configurable iteration limits, and comparing different compiler configurations.

## Compilation

To compile the benchmark runner:

```bash
make
```

## Running the Pre-Flatten vs. Head Benchmark

We provide a wrapper script, `run_pre_flatten_vs_head.sh`, which compares baseline MLton vs. MLton with pre-flattening enabled (the same configuration as `benchmark/run_pre_flatten_vs_head.sh`).

### Usage

The script requires a `--name` argument to identify the run. The results are saved as a JSONL (JSON Lines) file under the `outputs/` directory.

#### 1. Full Benchmark Run
To run the full suite with default benchmark counts:

```bash
./run_pre_flatten_vs_head.sh --name=my_benchmark_run
```

This will run all benchmarks and output the JSONL file to `outputs/my_benchmark_run:HOSTNAME:GIT_HASH:YYYYMMDD_HHMMSS.jsonl`.

#### 2. Quick Verification Run (Recommended for testing)
To verify that everything compiles and runs correctly without waiting for the full iteration counts (which can take up to an hour), you can restrict each benchmark to a single iteration and a single trial run:

```bash
./run_pre_flatten_vs_head.sh --name=init_test -max-bench-count 1 -once
```

### Passing Extra Arguments

You can pass extra arguments to the benchmark binary by appending them to the script invocation. For example, to run only a subset of benchmarks or specify other runner-specific options:

- To run only the `fib` and `fft` benchmarks:
  ```bash
  ./run_pre_flatten_vs_head.sh --name=subset_test -max-bench-count 1 -once fib fft
  ```
- To run with a custom benchmark count limit:
  ```bash
  ./run_pre_flatten_vs_head.sh --name=limit_test -max-bench-count 10
  ```
