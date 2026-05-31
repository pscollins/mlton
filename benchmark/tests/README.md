# MLton Benchmark Suite

This directory contains the Standard ML source files for the MLton benchmark suite.

## Prerequisites

Before running any benchmarks, you must first compile the benchmark runner tool in the parent directory:

```bash
cd /home/pscollins/code/mlton/benchmark
make
```

This generates the `benchmark` runner executable.

## Running Benchmarks

**Crucial:** You must execute the `benchmark` runner from within this `tests` directory so that it can locate the source `.sml` files.

```bash
cd /home/pscollins/code/mlton/benchmark/tests
../benchmark [options] <bench1> <bench2> ...
```

### Examples

1. **Run a single benchmark (`fib`) once using the local MLton build:**
   ```bash
   ../benchmark -mlton ../../build/bin/mlton -once fib
   ```

2. **Compare two different MLton binaries:**
   ```bash
   ../benchmark -mlton ../../build/bin/mlton -mlton /usr/bin/mlton -once fib
   ```

3. **Compare different optimization or code generator options using brace expansion:**
   ```bash
   ../benchmark -mlton "../../build/bin/mlton -codegen {c,amd64}" -once fib
   ```

4. **Run all benchmarks defined in the benchmark suite:**
   ```bash
   # From the benchmark root directory:
   make test
   ```

## Command-Line Options

* `-mlton "<path-to-mlton> [compile-args]"`: Specifies an MLton compiler instance. You can provide this option multiple times to compare different compiler builds or flag configurations.
* `-once`: Run each benchmark run only once. (By default, the runner repeats trials until the cumulative execution time exceeds 60 seconds to compute a stable average).
* `-args "<arguments>"`: Specifies space-separated arguments to pass to the benchmark programs.
* `-mlkit`, `-mosml`, `-poly`, `-smlnj`: Include other SML compilers in the comparison (if installed and present in your `PATH`).
* `-wiki`: Output results in wiki-markup table format.

## Cleanup

To clean up all temporary generated files and binaries:
```bash
make clean
```
