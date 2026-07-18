(* Unit tests for formatResults in lib.sml *)

val _ = print "Running lib-tests...\n"

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
            results = []
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
            results = []
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
            results = []
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
        
        val resultsData = [
            {bench = "fib", cmd = "mlton", compilerAbbrev = "MLton", compileTime = SOME 1.25, runTime = SOME 0.10, binarySize = SOME (Int64.fromInt 1024)},
            {bench = "fib", cmd = "gcc", compilerAbbrev = "GCC", compileTime = SOME 0.50, runTime = SOME 0.20, binarySize = SOME (Int64.fromInt 512)},
            {bench = "matrix", cmd = "mlton", compilerAbbrev = "MLton", compileTime = SOME 2.40, runTime = SOME 0.80, binarySize = SOME (Int64.fromInt 2048)}
        ]

        val output = BenchmarkLib.formatResults {
            compilers = compilers,
            benchmarks = benchmarks,
            failures = [],
            doWiki = false,
            showAll = false,
            results = resultsData
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
        val resultsData = [
            {bench = "fib", cmd = "gcc", compilerAbbrev = "GCC", compileTime = NONE, runTime = SOME 0.20, binarySize = NONE}
        ]

        val output = BenchmarkLib.formatResults {
            compilers = compilers,
            benchmarks = benchmarks,
            failures = [],
            doWiki = false,
            showAll = false,
            results = resultsData
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
        
        val resultsData = [
            {bench = "fib", cmd = "mlton", compilerAbbrev = "MLton", compileTime = SOME 1.25, runTime = SOME 0.10, binarySize = SOME (Int64.fromInt 1024)},
            {bench = "fib", cmd = "gcc", compilerAbbrev = "GCC", compileTime = SOME 0.50, runTime = SOME 0.20, binarySize = SOME (Int64.fromInt 512)},
            {bench = "matrix", cmd = "mlton", compilerAbbrev = "MLton", compileTime = SOME 2.40, runTime = SOME 0.80, binarySize = SOME (Int64.fromInt 2048)}
        ]

        val output = BenchmarkLib.formatResults {
            compilers = compilers,
            benchmarks = benchmarks,
            failures = [],
            doWiki = true,
            showAll = false,
            results = resultsData
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

(* Test 7: formatResult for a single record *)
val _ = runTest ("formatResult formatting", fn () =>
    let
        val res = {bench = "fib", cmd = "mlton", compilerAbbrev = "MLton", compileTime = SOME 1.25, runTime = SOME 0.10, binarySize = SOME (Int64.fromInt 1024)}
        val output = BenchmarkLib.formatResult res
        val expected = "fib (MLton) compile: 1.25s, run: 0.10s, size: 1,024"
    in
        assertEqual ("formatResult mismatch", output, expected)
    end)

(* Test 8: formatResult formatting with NONE *)
val _ = runTest ("formatResult formatting with NONE", fn () =>
    let
        val res = {bench = "matrix", cmd = "gcc", compilerAbbrev = "GCC", compileTime = NONE, runTime = NONE, binarySize = NONE}
        val output = BenchmarkLib.formatResult res
        val expected = "matrix (GCC) compile: *s, run: *s, size: *"
    in
        assertEqual ("formatResult with NONE mismatch", output, expected)
    end)

(* Test 9: benchCount for valid benchmark *)
val _ = runTest ("benchCount valid benchmark", fn () =>
    let
        val output = BenchmarkLib.benchCount "fib"
        val expected = "32"
    in
        assertEqual ("benchCount fib mismatch", output, expected)
    end)

(* Test 10: benchCount for another valid benchmark *)
val _ = runTest ("benchCount valid benchmark 2", fn () =>
    let
        val output = BenchmarkLib.benchCount "barnes-hut"
        val expected = "32768"
    in
        assertEqual ("benchCount barnes-hut mismatch", output, expected)
    end)

(* Test 11: benchCount for invalid benchmark raises Fail *)
val _ = runTest ("benchCount invalid benchmark", fn () =>
    let
        val gotExpectedExn = ref false
        val _ = (let val _ = BenchmarkLib.benchCount "non-existent-benchmark" in () end)
                handle Fail msg =>
                   if msg = "no benchCount for non-existent-benchmark"
                      then gotExpectedExn := true
                   else ()
    in
        assert ("benchCount should raise Fail with expected message", !gotExpectedExn)
    end)

(* Test 12: maxBenchCount is initially NONE *)
val _ = runTest ("maxBenchCount default value is NONE", fn () =>
    case !BenchmarkLib.maxBenchCount of
        NONE => ()
      | SOME _ => raise TestFail "maxBenchCount is not initially NONE")

(* Test 13: benchCount with maxBenchCount = SOME 10 (capping active) *)
val _ = runTest ("benchCount capped by maxBenchCount (SOME 10)", fn () =>
    let
        val _ = BenchmarkLib.maxBenchCount := SOME 10
        val outputFib = BenchmarkLib.benchCount "fib"
        val outputBarnes = BenchmarkLib.benchCount "barnes-hut"
        val _ = BenchmarkLib.maxBenchCount := NONE
    in
        assertEqual ("fib capped by 10", outputFib, "10");
        assertEqual ("barnes-hut capped by 10", outputBarnes, "10")
    end handle e => (BenchmarkLib.maxBenchCount := NONE; raise e))

(* Test 14: benchCount with maxBenchCount = SOME 100 (partially active) *)
val _ = runTest ("benchCount capped by maxBenchCount (SOME 100)", fn () =>
    let
        val _ = BenchmarkLib.maxBenchCount := SOME 100
        val outputFib = BenchmarkLib.benchCount "fib"
        val outputBarnes = BenchmarkLib.benchCount "barnes-hut"
        val _ = BenchmarkLib.maxBenchCount := NONE
    in
        assertEqual ("fib not capped by 100", outputFib, "32");
        assertEqual ("barnes-hut capped by 100", outputBarnes, "100")
    end handle e => (BenchmarkLib.maxBenchCount := NONE; raise e))

(* Test 15: benchCount with maxBenchCount = SOME 50000 (inactive capping) *)
val _ = runTest ("benchCount not capped by maxBenchCount (SOME 50000)", fn () =>
    let
        val _ = BenchmarkLib.maxBenchCount := SOME 50000
        val outputFib = BenchmarkLib.benchCount "fib"
        val outputBarnes = BenchmarkLib.benchCount "barnes-hut"
        val _ = BenchmarkLib.maxBenchCount := NONE
    in
        assertEqual ("fib not capped by 50000", outputFib, "32");
        assertEqual ("barnes-hut not capped by 50000", outputBarnes, "32768")
    end handle e => (BenchmarkLib.maxBenchCount := NONE; raise e))

(* Test 16: benchCount with maxBenchCount = SOME 0 *)
val _ = runTest ("benchCount capped by maxBenchCount (SOME 0)", fn () =>
    let
        val _ = BenchmarkLib.maxBenchCount := SOME 0
        val outputFib = BenchmarkLib.benchCount "fib"
        val _ = BenchmarkLib.maxBenchCount := NONE
    in
        assertEqual ("fib capped by 0", outputFib, "0")
    end handle e => (BenchmarkLib.maxBenchCount := NONE; raise e))

val _ = summarize ()
