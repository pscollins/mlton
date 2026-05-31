
(* Utilities for running unit tests *)

(* Titles of passed tests *)
val passedTests: string list ref = ref []
(* Titles + reasons of failed *)
val failedTests: (string * string) list ref = ref  []
(* Titles of disabled tests *)
val disabledTests: string list ref = ref []

exception TestFail of string

(* Runs `thunk` and catches `TestFail` to indicate failure: passing tests are
collected in `passedTests` and failing tests + failing reasons in `failedTests`

Unexpected exceptions are treated as test failures. *)
fun runTest (title: string, thunk: unit -> unit): unit =
    (print ("Running test: " ^ title ^ "\n");
     thunk (); passedTests := title :: !passedTests)
    handle TestFail reason =>
        (print ("Test failed: " ^ title ^ " - " ^ reason ^ "\n");
         failedTests := (title, reason) :: !failedTests)
    | exn =>
        let
            val reason = "Unexpected exception: " ^ exnMessage exn
        in
            print ("Test failed: " ^ title ^ " - " ^ reason ^ "\n");
            failedTests := (title, reason) :: !failedTests
        end

fun runTestDisabled (title: string, _: unit -> unit): unit =
    disabledTests := title :: !disabledTests

exception SuiteFail
(* Summarizes the result of this run on stdout and raises SuiteFail for any failures *)
fun summarize(): unit =
    let
        val numPassed = length (!passedTests)
        val numFailed = length (!failedTests)
        val numDisabled = length (!disabledTests)
        val total = numPassed + numFailed + numDisabled
        val _ = print ("Summary of " ^ Int.toString total ^ " tests:\n")
        val _ = app (fn title => print ("PASS: " ^ title ^ "\n")) (rev (!passedTests))
        val _ = app (fn (title, reason) => print ("FAIL: " ^ title ^ " (" ^ reason ^ ")\n")) (rev (!failedTests))
        val _ = app (fn title => print ("DISABLED: " ^ title ^ "\n")) (rev (!disabledTests))
        val _ = passedTests := []
        val _ = failedTests := []
        val _ = disabledTests := []
        val _ = if numDisabled > 0 then print ("WARNING: " ^ Int.toString numDisabled ^ " TESTS DID NOT RUN!\n") else ()
    in
        if numFailed > 0 then raise SuiteFail else ()
    end
    
