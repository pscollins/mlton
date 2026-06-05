(* RUN: mpl-print-c %s > %t || true

   Test that we can locate a `sourceMarkValue` in the generated C

   RUN: grep '// Diagnostic(Trace_staticSourceMarkValue:markX (0x1:w32))' %t
   RUN: grep '// Diagnostic(Trace_staticSourceMarkValue:markY (0x2:w32))' %t
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
