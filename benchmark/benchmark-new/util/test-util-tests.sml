(* Unit tests for test-util.sml *)

fun assert (msg, cond) =
    if cond then ()
    else (print ("Assertion failed: " ^ msg ^ "\n"); OS.Process.exit OS.Process.failure)

val _ = print "Running test-util-tests...\n"

(* Test runTest with passing thunk *)
val _ = (
    print "Testing runTest (pass)...\n";
    passedTests := [];
    failedTests := [];
    runTest ("PassTest", fn () => ());
    assert ("PassTest should be in passedTests", List.exists (fn s => s = "PassTest") (!passedTests));
    assert ("failedTests should be empty", List.length (!failedTests) = 0)
)

(* Test runTest with failing thunk *)
val _ = (
    print "Testing runTest (fail)...\n";
    passedTests := [];
    failedTests := [];
    runTest ("FailTest", fn () => raise TestFail "reason");
    assert ("FailTest should be in failedTests", List.exists (fn (s, r) => s = "FailTest" andalso r = "reason") (!failedTests));
    assert ("passedTests should be empty", List.length (!passedTests) = 0)
)

(* Test runTest with unexpected exception *)
val _ = (
    print "Testing runTest (unexpected exception)...\n";
    passedTests := [];
    failedTests := [];
    (runTest ("UnexpectedExceptionTest", fn () => raise Fail "unexpected")
     handle _ => ());
    assert ("UnexpectedExceptionTest should be in failedTests", List.exists (fn (s, _) => s = "UnexpectedExceptionTest") (!failedTests));
    assert ("passedTests should be empty", List.length (!passedTests) = 0)
)

(* Test summarize with success *)
val _ = (
    print "Testing summarize (success)...\n";
    passedTests := ["Test1"];
    failedTests := [];
    summarize ();
    assert ("passedTests should be reset", List.length (!passedTests) = 0);
    assert ("failedTests should be reset", List.length (!failedTests) = 0)
)

(* Test summarize with failure *)
val _ = (
    print "Testing summarize (failure)...\n";
    passedTests := [];
    failedTests := [("Test2", "fail")];
    let
        val raised = ref false
        val _ = summarize () handle SuiteFail => raised := true
    in
        assert ("summarize should raise SuiteFail on failures", !raised);
        assert ("passedTests should be reset after failure", List.length (!passedTests) = 0);
        assert ("failedTests should be reset after failure", List.length (!failedTests) = 0)
    end
)

(* Test runTestDisabled *)
val _ = (
    print "Testing runTestDisabled...\n";
    passedTests := [];
    failedTests := [];
    disabledTests := [];
    runTestDisabled ("DisabledTest", fn () => raise TestFail "should not run");
    assert ("DisabledTest should be in disabledTests", List.exists (fn s => s = "DisabledTest") (!disabledTests));
    assert ("passedTests should be empty", List.length (!passedTests) = 0);
    assert ("failedTests should be empty", List.length (!failedTests) = 0)
)

(* Test summarize with disabled tests *)
val _ = (
    print "Testing summarize (with disabled)...\n";
    passedTests := ["Test1"];
    failedTests := [];
    disabledTests := ["Test2"];
    summarize ();
    assert ("passedTests should be reset", List.length (!passedTests) = 0);
    assert ("failedTests should be reset", List.length (!failedTests) = 0);
    assert ("disabledTests should be reset", List.length (!disabledTests) = 0)
)

val _ = print "All test-util-tests completed (unexpectedly if using stubs)!\n"
