(* RUN: mpl-compile -keep-pass annotateTraceValue %s %t || true

   Test sourceMarkValue -> staticSourceMarkValue conversion

   Expect static version in `.post`
   RUN: grep 'Trace_staticSourceMarkValue:markX\[int32\] (kX_0)' %t/*annotateTraceValue.post.core-ml
   RUN: grep 'Trace_staticSourceMarkValue:markY\[int32\] (kY_0)' %t/*annotateTraceValue.post.core-ml
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
