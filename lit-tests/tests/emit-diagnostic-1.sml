(* RUN: mpl-compile -keep-pass emitDiagnostics %s %t || true

   Test `Trace_staticSourceMark` -> `Diagnostic` conversion

   PrimApp in `pre`
   RUN: grep 'Trace_staticSourceMark:mark1' %t/*emitDiagnostics.pre.machine
   RUN: grep 'Trace_staticSourceMark:mark2' %t/*emitDiagnostics.pre.machine

   Diagnostic in `post`
   RUN: grep 'Diagnostic(Trace_staticSourceMark:mark1 ())' %t/*emitDiagnostics.post.machine
   RUN: grep 'Diagnostic(Trace_staticSourceMark:mark2 ())' %t/*emitDiagnostics.post.machine
 *)

val _ = let
    val kConst = 123456789
    val m1 = MLton.Trace.sourceMark "mark1"
    val kConst2 = kConst + 1
    val m2 = MLton.Trace.sourceMark "mark2"
in
    print (Int.toString kConst2)
end
