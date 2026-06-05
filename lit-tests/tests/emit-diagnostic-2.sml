(* RUN: mpl-compile -keep-pass emitDiagnostics %s %t || true

   Test `Trace_staticSourceMarkValue` -> `Diagnostic` conversion

   Diagnostic in `post`:
   RUN: grep 'Diagnostic(Trace_staticSourceMarkValue:markX (0x1:w32))' %t/*emitDiagnostics.post.machine
   RUN: grep 'Diagnostic(Trace_staticSourceMarkValue:markY (0x2:w32))' %t/*emitDiagnostics.post.machine
 *)

val _ = let
    val kX = 1
    val kY = 2
    val _ = MLton.Trace.sourceMarkValue (kX, "markX")
    val _ = MLton.Trace.sourceMarkValue (kY, "markY")
    val z = kX + kY
in
    print (Int.toString z)
end
