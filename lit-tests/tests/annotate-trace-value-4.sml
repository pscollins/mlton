(* RUN: mpl-compile -keep-pass annotateTraceValue %s %t || true

   Test sourceMarkValue -> staticSourceMarkValue conversion

   (Test case for understanding `DeepFlatten`)

   Expect static version in `.post`
   RUN: grep 'Trace_staticSourceMarkValue:px1' %t/*annotateTraceValue.post.core-ml
   RUN: grep 'Trace_staticSourceMarkValue:py1' %t/*annotateTraceValue.post.core-ml
   RUN: grep 'Trace_staticSourceMarkValue:px2' %t/*annotateTraceValue.post.core-ml
   RUN: grep 'Trace_staticSourceMarkValue:py2' %t/*annotateTraceValue.post.core-ml
 *)



val _ = let
   fun mkElement idx = (idx, idx + 1)
   val arr = Array.tabulate (2, mkElement)
   val p1 = Array.sub (arr, 0)
   val x1 = #1 p1
   val y1 = #2 p1
   val _ = MLton.Trace.sourceMarkValue (x1, "px1")
   val _ = MLton.Trace.sourceMarkValue (y1, "py1")

   val p2 = Array.sub (arr, 1)
   val x2 = #1 p2
   val y2 = #2 p2
   val _ = MLton.Trace.sourceMarkValue (x2, "px2")
   val _ = MLton.Trace.sourceMarkValue (y2, "py2")
in
   print (Int.toString (x1 + y1 + x2 + y2))
end
