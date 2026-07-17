(* Unit tests for formatResults in lib.sml *)

val _ = print "Running format-results-tests...\n"

fun assert (msg, cond) =
    if cond then ()
    else raise TestFail msg

fun assertEqual (msg, actual, expected) =
    if actual = expected then ()
    else (
        print ("\n=== ASSERTION FAILED: " ^ msg ^ " ===\n");
        print "--- EXPECTED ---\n";
        print expected;
        print "--- ACTUAL ---\n";
        print actual;
        print "===============\n\n";
        raise TestFail msg
    )

(* Base inputs for tests *)
val singleCompiler = [{name = "MLton Compiler", abbrv = "MLton"}]
val singleBenchmark = ["fib"]

(* Test 1: Empty results / minimal input *)
val _ = runTest ("Empty results", fn () =>
    let
        val output = BenchmarkLib.formatResults {
            compilers = singleCompiler,
            benchmarks = singleBenchmark,
            failures = [],
            doWiki = false,
            showAll = false,
            results = {compiles = [], runs = [], sizes = []}
        }
        val expected =
            "MLton -- MLton Compiler\n" ^
            "run time ratio\n" ^
            "benchmark\n" ^
            "size\n" ^
            "benchmark\n" ^
            "compile time\n" ^
            "benchmark\n" ^
            "run time\n" ^
            "benchmark\n"
    in
        assertEqual ("Output mismatch", output, expected)
    end)

(* Test 2: Empty results with showAll = true *)
val _ = runTest ("Empty results with showAll", fn () =>
    let
        val output = BenchmarkLib.formatResults {
            compilers = singleCompiler,
            benchmarks = singleBenchmark,
            failures = [],
            doWiki = false,
            showAll = true,
            results = {compiles = [], runs = [], sizes = []}
        }
        val table =
            "benchmark MLton\n" ^
            "fib           *\n"
        val expected =
            "MLton -- MLton Compiler\n" ^
            "run time ratio\n" ^
            table ^
            "size\n" ^
            table ^
            "compile time\n" ^
            table ^
            "run time\n" ^
            table
    in
        assertEqual ("Output mismatch for showAll", output, expected)
    end)

(* Test 3: Failure warning message *)
val _ = runTest ("Failure warning formatting", fn () =>
    let
        val output = BenchmarkLib.formatResults {
            compilers = singleCompiler,
            benchmarks = singleBenchmark,
            failures = ["fib", "matrix"],
            doWiki = false,
            showAll = false,
            results = {compiles = [], runs = [], sizes = []}
        }
        val expected =
            "MLton -- MLton Compiler\n" ^
            "WARNING: MLton failed on: fib, matrix\n" ^
            "run time ratio\n" ^
            "benchmark\n" ^
            "size\n" ^
            "benchmark\n" ^
            "compile time\n" ^
            "benchmark\n" ^
            "run time\n" ^
            "benchmark\n"
    in
        assertEqual ("Failure warning output mismatch", output, expected)
    end)

(* Test 4: Full data representation (showAll = false) *)
val _ = runTest ("Full data (showAll = false)", fn () =>
    let
        val compilers = [
            {name = "MLton Compiler", abbrv = "MLton"},
            {name = "GCC Compiler", abbrv = "GCC"}
        ]
        val benchmarks = ["fib", "matrix"]
        
        val compilesData = [
            {bench = "fib", compiler = "MLton", value = 1.25},
            {bench = "fib", compiler = "GCC", value = 0.50},
            {bench = "matrix", compiler = "MLton", value = 2.40}
        ]
        val sizesData = [
            {bench = "fib", compiler = "MLton", value = Position.fromInt 1024},
            {bench = "fib", compiler = "GCC", value = Position.fromInt 512},
            {bench = "matrix", compiler = "MLton", value = Position.fromInt 2048}
        ]
        val runsData = [
            {bench = "fib", compiler = "MLton", value = 0.10},
            {bench = "fib", compiler = "GCC", value = 0.20},
            {bench = "matrix", compiler = "MLton", value = 0.80}
        ]

        val output = BenchmarkLib.formatResults {
            compilers = compilers,
            benchmarks = benchmarks,
            failures = [],
            doWiki = false,
            showAll = false,
            results = {compiles = compilesData, runs = runsData, sizes = sizesData}
        }

        val expected =
            "MLton -- MLton Compiler\n" ^
            "GCC -- GCC Compiler\n" ^
            "run time ratio\n" ^
            "benchmark  GCC\n" ^
            "fib       2.00\n" ^
            "size\n" ^
            "benchmark MLton GCC\n" ^
            "fib       1,024 512\n" ^
            "matrix    2,048   *\n" ^
            "compile time\n" ^
            "benchmark MLton  GCC\n" ^
            "fib        1.25 0.50\n" ^
            "matrix     2.40    *\n" ^
            "run time\n" ^
            "benchmark MLton  GCC\n" ^
            "fib        0.10 0.20\n" ^
            "matrix     0.80    *\n"
    in
        assertEqual ("Full data (showAll = false) output mismatch", output, expected)
    end)

(* Test 5: Missing baseline for ratio (should result in ~1.00) *)
val _ = runTest ("Missing baseline ratio (~1.00)", fn () =>
    let
        val compilers = [
            {name = "MLton Compiler", abbrv = "MLton"},
            {name = "GCC Compiler", abbrv = "GCC"}
        ]
        val benchmarks = ["fib"]
        
        (* Runs has GCC run but NO MLton (base) run for fib *)
        val runsData = [
            {bench = "fib", compiler = "GCC", value = 0.20}
        ]

        val output = BenchmarkLib.formatResults {
            compilers = compilers,
            benchmarks = benchmarks,
            failures = [],
            doWiki = false,
            showAll = false,
            results = {compiles = [], runs = runsData, sizes = []}
        }

        val expected =
            "MLton -- MLton Compiler\n" ^
            "GCC -- GCC Compiler\n" ^
            "run time ratio\n" ^
            "benchmark   GCC\n" ^
            "fib       ~1.00\n" ^
            "size\n" ^
            "benchmark\n" ^
            "compile time\n" ^
            "benchmark\n" ^
            "run time\n" ^
            "benchmark  GCC\n" ^
            "fib       0.20\n"
    in
        assertEqual ("Missing baseline ratio output mismatch", output, expected)
    end)

(* Test 6: Wiki formatting *)
val _ = runTest ("Wiki formatting option", fn () =>
    let
        val compilers = [
            {name = "MLton Compiler", abbrv = "MLton"},
            {name = "GCC Compiler", abbrv = "GCC"}
        ]
        val benchmarks = ["fib", "matrix"]
        
        val compilesData = [
            {bench = "fib", compiler = "MLton", value = 1.25},
            {bench = "fib", compiler = "GCC", value = 0.50},
            {bench = "matrix", compiler = "MLton", value = 2.40}
        ]
        val sizesData = [
            {bench = "fib", compiler = "MLton", value = Position.fromInt 1024},
            {bench = "fib", compiler = "GCC", value = Position.fromInt 512},
            {bench = "matrix", compiler = "MLton", value = Position.fromInt 2048}
        ]
        val runsData = [
            {bench = "fib", compiler = "MLton", value = 0.10},
            {bench = "fib", compiler = "GCC", value = 0.20},
            {bench = "matrix", compiler = "MLton", value = 0.80}
        ]

        val output = BenchmarkLib.formatResults {
            compilers = compilers,
            benchmarks = benchmarks,
            failures = [],
            doWiki = true,
            showAll = false,
            results = {compiles = compilesData, runs = runsData, sizes = sizesData}
        }

        (* Standard output from before *)
        val standardPart =
            "MLton -- MLton Compiler\n" ^
            "GCC -- GCC Compiler\n" ^
            "run time ratio\n" ^
            "benchmark  GCC\n" ^
            "fib       2.00\n"

        (* Wiki part for run time ratio.
           Note: toStringHtml is r2s 1. So value 2.0 is formatted to "2.0".
         *)
        val wikiRatio =
            "||benchmark||GCC||\n" ^
            "||[attachment:fib.sml fib]||2.0||\n"

        val standardSize =
            "size\n" ^
            "benchmark MLton GCC\n" ^
            "fib       1,024 512\n" ^
            "matrix    2,048   *\n"

        val wikiSize =
            "||benchmark||MLton||GCC||\n" ^
            "||[attachment:fib.sml fib]||1,024||512||\n" ^
            "||[attachment:matrix.sml matrix]||2,048||*||\n"

        val standardCompile =
            "compile time\n" ^
            "benchmark MLton  GCC\n" ^
            "fib        1.25 0.50\n" ^
            "matrix     2.40    *\n"

        val wikiCompile =
            "||benchmark||MLton||GCC||\n" ^
            "||[attachment:fib.sml fib]||1.25||0.50||\n" ^
            "||[attachment:matrix.sml matrix]||2.40||*||\n"

        val standardRun =
            "run time\n" ^
            "benchmark MLton  GCC\n" ^
            "fib        0.10 0.20\n" ^
            "matrix     0.80    *\n"

        val wikiRun =
            "||benchmark||MLton||GCC||\n" ^
            "||[attachment:fib.sml fib]||0.10||0.20||\n" ^
            "||[attachment:matrix.sml matrix]||0.80||*||\n"

        val expected =
            standardPart ^ wikiRatio ^
            standardSize ^ wikiSize ^
            standardCompile ^ wikiCompile ^
            standardRun ^ wikiRun
    in
        assertEqual ("Wiki formatting output mismatch", output, expected)
    end)

val _ = summarize ()
