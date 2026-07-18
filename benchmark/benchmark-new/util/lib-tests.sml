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
        val output = BenchmarkLib.formatResults BenchmarkLib.legacyRow {
            compilers = singleCompiler,
            benchmarks = singleBenchmark,
            failures = [],
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
        val output = BenchmarkLib.formatResults BenchmarkLib.legacyRow {
            compilers = singleCompiler,
            benchmarks = singleBenchmark,
            failures = [],
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
        val output = BenchmarkLib.formatResults BenchmarkLib.legacyRow {
            compilers = singleCompiler,
            benchmarks = singleBenchmark,
            failures = ["fib", "matrix"],
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
            {bench = "fib", cmd = "mlton", compilerAbbrev = "MLton", compileTime = SOME 1.25, runTime = SOME 0.10, binarySize = SOME (Int64.fromInt 1024),
             binaryChecksum = SOME "hash1", hostname = "host1", timestamp = "2026-07-18 20:00:00", commitHash = "commit1"},
            {bench = "fib", cmd = "gcc", compilerAbbrev = "GCC", compileTime = SOME 0.50, runTime = SOME 0.20, binarySize = SOME (Int64.fromInt 512),
             binaryChecksum = SOME "hash2", hostname = "host2", timestamp = "2026-07-18 20:00:01", commitHash = "commit2"},
            {bench = "matrix", cmd = "mlton", compilerAbbrev = "MLton", compileTime = SOME 2.40, runTime = SOME 0.80, binarySize = SOME (Int64.fromInt 2048),
             binaryChecksum = NONE, hostname = "host1", timestamp = "2026-07-18 20:00:02", commitHash = "commit1"}
        ]

        val output = BenchmarkLib.formatResults BenchmarkLib.legacyRow {
            compilers = compilers,
            benchmarks = benchmarks,
            failures = [],
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
            {bench = "fib", cmd = "gcc", compilerAbbrev = "GCC", compileTime = NONE, runTime = SOME 0.20, binarySize = NONE,
             binaryChecksum = NONE, hostname = "host2", timestamp = "2026-07-18 20:00:01", commitHash = "commit2"}
        ]

        val output = BenchmarkLib.formatResults BenchmarkLib.legacyRow {
            compilers = compilers,
            benchmarks = benchmarks,
            failures = [],
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

(* Test 6a: formatResults with jsonRow on empty results *)
val _ = runTest ("formatResults empty JSON", fn () =>
    let
        val output = BenchmarkLib.formatResults BenchmarkLib.jsonRow {
            compilers = singleCompiler,
            benchmarks = singleBenchmark,
            failures = [],
            showAll = false,
            results = []
        }
        val expected = ""
    in
        assertEqual ("JSON output mismatch", output, expected)
    end)

(* Test 6b: formatResults with jsonRow on full data *)
val _ = runTest ("formatResults full JSON", fn () =>
    let
        val compilers = [
            {name = "MLton Compiler", abbrv = "MLton"},
            {name = "GCC Compiler", abbrv = "GCC"}
        ]
        val benchmarks = ["fib", "matrix"]
        
        val resultsData = [
            {bench = "fib", cmd = "mlton", compilerAbbrev = "MLton", compileTime = SOME 1.25, runTime = SOME 0.10, binarySize = SOME (Int64.fromInt 1024),
             binaryChecksum = SOME "hash1", hostname = "host1", timestamp = "time1", commitHash = "commit1"},
            {bench = "fib", cmd = "gcc", compilerAbbrev = "GCC", compileTime = SOME 0.50, runTime = SOME 0.20, binarySize = SOME (Int64.fromInt 512),
             binaryChecksum = SOME "hash2", hostname = "host2", timestamp = "time2", commitHash = "commit2"},
            {bench = "matrix", cmd = "mlton", compilerAbbrev = "MLton", compileTime = SOME 2.40, runTime = SOME 0.80, binarySize = SOME (Int64.fromInt 2048),
             binaryChecksum = NONE, hostname = "host1", timestamp = "time3", commitHash = "commit1"}
        ]

        val output = BenchmarkLib.formatResults BenchmarkLib.jsonRow {
            compilers = compilers,
            benchmarks = benchmarks,
            failures = [],
            showAll = false,
            results = resultsData
        }

        val expected =
            "{\"bench\":\"fib\",\"cmd\":\"mlton\",\"compilerAbbrev\":\"MLton\",\"compileTime\":0.125E1,\"runTime\":0.1,\"binarySize\":1024,\"binaryChecksum\":\"hash1\",\"hostname\":\"host1\",\"timestamp\":\"time1\",\"commitHash\":\"commit1\"}\n" ^
            "{\"bench\":\"fib\",\"cmd\":\"gcc\",\"compilerAbbrev\":\"GCC\",\"compileTime\":0.5,\"runTime\":0.2,\"binarySize\":512,\"binaryChecksum\":\"hash2\",\"hostname\":\"host2\",\"timestamp\":\"time2\",\"commitHash\":\"commit2\"}\n" ^
            "{\"bench\":\"matrix\",\"cmd\":\"mlton\",\"compilerAbbrev\":\"MLton\",\"compileTime\":0.24E1,\"runTime\":0.8,\"binarySize\":2048,\"binaryChecksum\":null,\"hostname\":\"host1\",\"timestamp\":\"time3\",\"commitHash\":\"commit1\"}\n"
    in
        assertEqual ("JSON output mismatch", output, expected)
    end)

(* Test 7a: formatResult for a single record (legacy) *)
val _ = runTest ("formatResult formatting (legacy)", fn () =>
    let
        val res = {bench = "fib", cmd = "mlton", compilerAbbrev = "MLton", compileTime = SOME 1.25, runTime = SOME 0.10, binarySize = SOME (Int64.fromInt 1024),
                   binaryChecksum = SOME "hash1", hostname = "host1", timestamp = "time1", commitHash = "commit1"}
        val output = BenchmarkLib.formatResult BenchmarkLib.legacyRow res
        val expected = "fib (MLton) compile: 1.25s, run: 0.10s, size: 1,024"
    in
        assertEqual ("formatResult mismatch", output, expected)
    end)

(* Test 7b: formatResult for a single record (json) *)
val _ = runTest ("formatResult formatting (json)", fn () =>
    let
        val res = {bench = "fib", cmd = "mlton", compilerAbbrev = "MLton", compileTime = SOME 1.25, runTime = SOME 0.10, binarySize = SOME (Int64.fromInt 1024),
                   binaryChecksum = SOME "hash1", hostname = "host1", timestamp = "time1", commitHash = "commit1"}
        val output = BenchmarkLib.formatResult BenchmarkLib.jsonRow res
        val expected = "{\"bench\":\"fib\",\"cmd\":\"mlton\",\"compilerAbbrev\":\"MLton\",\"compileTime\":0.125E1,\"runTime\":0.1,\"binarySize\":1024,\"binaryChecksum\":\"hash1\",\"hostname\":\"host1\",\"timestamp\":\"time1\",\"commitHash\":\"commit1\"}"
    in
        assertEqual ("formatResult json mismatch", output, expected)
    end)

(* Test 8a: formatResult formatting with NONE (legacy) *)
val _ = runTest ("formatResult formatting with NONE (legacy)", fn () =>
    let
        val res = {bench = "matrix", cmd = "gcc", compilerAbbrev = "GCC", compileTime = NONE, runTime = NONE, binarySize = NONE,
                   binaryChecksum = NONE, hostname = "host1", timestamp = "time1", commitHash = "commit1"}
        val output = BenchmarkLib.formatResult BenchmarkLib.legacyRow res
        val expected = "matrix (GCC) compile: *s, run: *s, size: *"
    in
        assertEqual ("formatResult with NONE mismatch", output, expected)
    end)

(* Test 8b: formatResult formatting with NONE (json) *)
val _ = runTest ("formatResult formatting with NONE (json)", fn () =>
    let
        val res = {bench = "matrix", cmd = "gcc", compilerAbbrev = "GCC", compileTime = NONE, runTime = NONE, binarySize = NONE,
                   binaryChecksum = NONE, hostname = "host1", timestamp = "time1", commitHash = "commit1"}
        val output = BenchmarkLib.formatResult BenchmarkLib.jsonRow res
        val expected = "{\"bench\":\"matrix\",\"cmd\":\"gcc\",\"compilerAbbrev\":\"GCC\",\"compileTime\":null,\"runTime\":null,\"binarySize\":null,\"binaryChecksum\":null,\"hostname\":\"host1\",\"timestamp\":\"time1\",\"commitHash\":\"commit1\"}"
    in
        assertEqual ("formatResult json with NONE mismatch", output, expected)
    end)

(* Test 8c: formatResult formatting with escape characters (json) *)
val _ = runTest ("formatResult formatting with escape characters (json)", fn () =>
    let
        val res = {bench = "hello\"world\\", cmd = "run \"me\"", compilerAbbrev = "AB\"C", compileTime = SOME 0.05, runTime = SOME 0.15, binarySize = SOME (Int64.fromInt 256),
                   binaryChecksum = SOME "hash\"1", hostname = "host\"1", timestamp = "time\"1", commitHash = "commit\"1"}
        val output = BenchmarkLib.formatResult BenchmarkLib.jsonRow res
        val expected = "{\"bench\":\"hello\\\"world\\\\\",\"cmd\":\"run \\\"me\\\"\",\"compilerAbbrev\":\"AB\\\"C\",\"compileTime\":0.5E-1,\"runTime\":0.15,\"binarySize\":256,\"binaryChecksum\":\"hash\\\"1\",\"hostname\":\"host\\\"1\",\"timestamp\":\"time\\\"1\",\"commitHash\":\"commit\\\"1\"}"
    in
        assertEqual ("formatResult json escape mismatch", output, expected)
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

(* Test 17: maybeWriteToFile with NONE *)
val _ = runTest ("maybeWriteToFile with NONE", fn () =>
    let
        val testFile = "test_none.json"
        val _ = if File.doesExist testFile then File.remove testFile else ()
        val results = [{bench = "fib", cmd = "mlton", compilerAbbrev = "MLton", compileTime = SOME 1.25, runTime = SOME 0.10, binarySize = SOME (Int64.fromInt 1024),
                        binaryChecksum = SOME "hash1", hostname = "host1", timestamp = "time1", commitHash = "commit1"}]
        val _ = BenchmarkLib.maybeWriteToFile (NONE, results)
        val exists = File.doesExist testFile
    in
        assert ("File should not exist when path is NONE", not exists)
    end)

(* Test 18: maybeWriteToFile with SOME path and empty results *)
val _ = runTest ("maybeWriteToFile with SOME path and empty results", fn () =>
    let
        val testFile = "test_empty_results.json"
        val _ = if File.doesExist testFile then File.remove testFile else ()
        val _ = BenchmarkLib.maybeWriteToFile (SOME testFile, [])
        val exists = File.doesExist testFile
        val _ = assert ("File should exist", exists)
        val content = File.contents testFile
        val _ = File.remove testFile
    in
        assertEqual ("File content should be empty", content, "")
    end handle e => (if File.doesExist "test_empty_results.json" then File.remove "test_empty_results.json" else (); raise e))

(* Test 19: maybeWriteToFile with SOME path and non-empty results *)
val _ = runTest ("maybeWriteToFile with SOME path and non-empty results", fn () =>
    let
        val testFile = "test_results.json"
        val _ = if File.doesExist testFile then File.remove testFile else ()
        val results = [{bench = "fib", cmd = "mlton", compilerAbbrev = "MLton", compileTime = SOME 1.25, runTime = SOME 0.10, binarySize = SOME (Int64.fromInt 1024),
                        binaryChecksum = SOME "hash1", hostname = "host1", timestamp = "time1", commitHash = "commit1"}]
        val _ = BenchmarkLib.maybeWriteToFile (SOME testFile, results)
        val exists = File.doesExist testFile
        val _ = assert ("File should exist", exists)
        val content = File.contents testFile
        val expected = "{\"bench\":\"fib\",\"cmd\":\"mlton\",\"compilerAbbrev\":\"MLton\",\"compileTime\":0.125E1,\"runTime\":0.1,\"binarySize\":1024,\"binaryChecksum\":\"hash1\",\"hostname\":\"host1\",\"timestamp\":\"time1\",\"commitHash\":\"commit1\"}\n"
        val _ = File.remove testFile
    in
        assertEqual ("File content should match", content, expected)
    end handle e => (if File.doesExist "test_results.json" then File.remove "test_results.json" else (); raise e))

val _ = summarize ()
