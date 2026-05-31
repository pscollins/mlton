(* RUN: mlton-compile \
   RUN:    -keep-pass 'shallowFlatten.*' \
   RUN:    -shallow-flatten-policy maxWidth:2 \
   RUN:    -shallow-flatten-max-iters 1 \
   RUN:    %s %t
  
   Test that datatypes containing tuple-arrays are flattened.

   RUN: ! grep -F 'Array_alloc[(real64, real64) tuple]'  %t/*shallowFlatten*.post.ssa
   RUN: grep -F  'Array_alloc[real64]'  %t/*shallowFlatten*.post.ssa
 *)

datatype wrapper = Wrap of (real * real) array

val n = List.length (CommandLine.arguments ()) + 10
val r = Real.fromInt n

val arr = Array.array (n, (r, r + 1.0))
val w = Wrap arr

val (Wrap a) = w
val (x, y) = Array.sub (a, 0)
val _ = print (Real.toString x ^ " " ^ Real.toString y ^ "\n")
